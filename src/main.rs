use clap::{ArgGroup, Parser};
use flate2::read::{GzDecoder, ZlibDecoder};
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::time::Instant;

/// ---------------------------
/// CLI
/// ---------------------------
#[derive(Parser, Debug)]
#[command(name = "gtnh_finder", version, about = "Offline block/TE scanner -> JourneyMap waypoints (1.7.10)")]
#[command(group(
    ArgGroup::new("targets")
        .required(true)
        .args(["ids", "te_ids"])
))]
struct Cli {
    /// Path to the world save folder (contains 'region', 'DIM-1/region', etc.)
    #[arg(long)]
    world: PathBuf,

    /// OPTIONAL: JourneyMap 5.x 'waypoints' dir. If omitted, we only PREVIEW (print hits).
    #[arg(long)]
    jm_waypoints: Option<PathBuf>,

    /// Dimension id: 0=Overworld, -1=Nether, 1=End, etc.
    #[arg(long, default_value_t = 0)]
    dim: i32,

    /// Comma-separated numeric block IDs to search for (e.g. 3460,3459). Hex ok: 0xD84.
    #[arg(long)]
    ids: Option<String>,

    /// Comma-separated TileEntity ids (e.g. MobSpawner, Chest)
    #[arg(long)]
    te_ids: Option<String>,

    /// Min Y to scan (inclusive)
    #[arg(long, default_value_t = 0)]
    y_min: i32,

    /// Max Y to scan (inclusive)
    #[arg(long, default_value_t = 255)]
    y_max: i32,

    /// Skip writing if an existing waypoint is within this many blocks
    #[arg(long, default_value_t = 10)]
    near_threshold: i32,

    /// If >0, merge nearby hits to one waypoint (useful for dungeons)
    #[arg(long, default_value_t = 0)]
    cluster_radius: i32,

    /// Waypoint icon (used only when --write and --jm-waypoints are provided)
    #[arg(long, default_value = "temple")]
    icon: String,

    /// Waypoint label (default depends on search type)
    #[arg(long)]
    label: Option<String>,

    /// Waypoint color RGB, e.g. "255,255,255"
    #[arg(long, default_value = "255,255,255")]
    color: String,

    /// Write mode. If not set, we PREVIEW (print hits) and do not write files.
    #[arg(long)]
    write: bool,
}

/// ---------------------------
/// Region (.mca) reader
/// ---------------------------
const SECTOR_BYTES: u64 = 4096;
const COMPRESSION_GZIP: u8 = 1;
const COMPRESSION_ZLIB: u8 = 2;

struct RegionFile {
    file: File,
    locations: [u8; 4096],
    _timestamps: [u8; 4096],
}

impl RegionFile {
    fn open(p: &Path) -> io::Result<Self> {
        let mut file = File::open(p)?;
        let mut locations = [0u8; 4096];
        let mut timestamps = [0u8; 4096];
        file.read_exact(&mut locations)?;
        file.read_exact(&mut timestamps)?;
        Ok(Self { file, locations, _timestamps: timestamps })
    }

    fn get_chunk_offset_count(&self, index: usize) -> (u32, u8) {
        let i = index * 4;
        let off = ((self.locations[i] as u32) << 16)
            | ((self.locations[i + 1] as u32) << 8)
            | (self.locations[i + 2] as u32);
        let count = self.locations[i + 3];
        (off, count)
    }

    fn read_chunk(&mut self, cx: usize, cz: usize) -> io::Result<Option<Vec<u8>>> {
        if cx >= 32 || cz >= 32 {
            return Ok(None);
        }
        let index = cx + cz * 32;
        let (off, sectors) = self.get_chunk_offset_count(index);
        if off == 0 || sectors == 0 {
            return Ok(None);
        }
        self.file.seek(SeekFrom::Start(off as u64 * SECTOR_BYTES))?;
        let mut len_buf = [0u8; 4];
        self.file.read_exact(&mut len_buf)?;
        let length = u32::from_be_bytes(len_buf);
        let mut comp = [0u8; 1];
        self.file.read_exact(&mut comp)?;
        let mut compressed = vec![0u8; (length - 1) as usize];
        self.file.read_exact(&mut compressed)?;
        let data = match comp[0] {
            COMPRESSION_ZLIB => {
                let mut d = ZlibDecoder::new(&compressed[..]);
                let mut out = Vec::new();
                d.read_to_end(&mut out)?;
                out
            }
            COMPRESSION_GZIP => {
                let mut d = GzDecoder::new(&compressed[..]);
                let mut out = Vec::new();
                d.read_to_end(&mut out)?;
                out
            }
            _ => return Ok(None),
        };
        Ok(Some(data))
    }
}

fn region_dir(world: &Path, dim: i32) -> PathBuf {
    if dim == 0 {
        world.join("region")
    } else {
        world.join(format!("DIM{}", dim)).join("region")
    }
}

fn list_region_files(world: &Path, dim: i32) -> io::Result<Vec<PathBuf>> {
    let dir = region_dir(world, dim);
    let mut out = vec![];
    if dir.is_dir() {
        for e in fs::read_dir(dir)? {
            let p = e?.path();
            if p.extension().map(|s| s == "mca").unwrap_or(false) {
                out.push(p);
            }
        }
    }
    Ok(out)
}

/// ---------------------------
/// Minimal NBT reader (subset)
/// ---------------------------
#[repr(u8)]
enum Tag {
    End = 0,
    Byte = 1,
    Short = 2,
    Int = 3,
    Long = 4,
    Float = 5,
    Double = 6,
    ByteArray = 7,
    String = 8,
    List = 9,
    Compound = 10,
    IntArray = 11,
    LongArray = 12,
}

struct Cursor<'a> {
    buf: &'a [u8],
    pos: usize,
}
impl<'a> Cursor<'a> {
    fn new(b: &'a [u8]) -> Self { Self { buf: b, pos: 0 } }
    fn read_exact(&mut self, n: usize) -> io::Result<&'a [u8]> {
        if self.pos + n > self.buf.len() { return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "nbt eof")); }
        let s = &self.buf[self.pos..self.pos + n];
        self.pos += n;
        Ok(s)
    }
    fn read_u8(&mut self) -> io::Result<u8> { Ok(self.read_exact(1)?[0]) }
    fn read_i8(&mut self) -> io::Result<i8> { Ok(self.read_u8()? as i8) }
    fn read_u16(&mut self) -> io::Result<u16> { let mut a=[0;2]; a.copy_from_slice(self.read_exact(2)?); Ok(u16::from_be_bytes(a)) }
    fn read_i16(&mut self) -> io::Result<i16> { Ok(self.read_u16()? as i16) }
    fn read_u32(&mut self) -> io::Result<u32> { let mut a=[0;4]; a.copy_from_slice(self.read_exact(4)?); Ok(u32::from_be_bytes(a)) }
    fn read_i32(&mut self) -> io::Result<i32> { Ok(self.read_u32()? as i32) }
    fn read_u64(&mut self) -> io::Result<u64> { let mut a=[0;8]; a.copy_from_slice(self.read_exact(8)?); Ok(u64::from_be_bytes(a)) }
    fn read_i64(&mut self) -> io::Result<i64> { Ok(self.read_u64()? as i64) }
    fn read_f32(&mut self) -> io::Result<f32> { let mut a=[0;4]; a.copy_from_slice(self.read_exact(4)?); Ok(f32::from_bits(u32::from_be_bytes(a))) }
    fn read_f64(&mut self) -> io::Result<f64> { let mut a=[0;8]; a.copy_from_slice(self.read_exact(8)?); Ok(f64::from_bits(u64::from_be_bytes(a))) }
    fn read_string(&mut self) -> io::Result<String> {
        let n = self.read_u16()? as usize;
        let s = self.read_exact(n)?;
        Ok(String::from_utf8_lossy(s).into_owned())
    }
}

#[derive(Debug)]
enum Nbt {
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    ByteArray(Vec<u8>),
    String(String),
    List(u8, Vec<Nbt>),
    Compound(HashMap<String, Nbt>),
    IntArray(Vec<i32>),
    LongArray(Vec<i64>),
}

fn read_tag(cur: &mut Cursor) -> io::Result<(u8, String, Nbt)> {
    let tid = cur.read_u8()?;
    if tid == Tag::End as u8 {
        return Ok((tid, String::new(), Nbt::Byte(0)));
    }
    let name = cur.read_string()?;
    let payload = read_payload(cur, tid)?;
    Ok((tid, name, payload))
}
fn read_payload(cur: &mut Cursor, tid: u8) -> io::Result<Nbt> {
    use Nbt::*;
    match tid {
        x if x == Tag::Byte as u8 => Ok(Byte(cur.read_i8()?)),
        x if x == Tag::Short as u8 => Ok(Short(cur.read_i16()?)),
        x if x == Tag::Int as u8 => Ok(Int(cur.read_i32()?)),
        x if x == Tag::Long as u8 => Ok(Long(cur.read_i64()?)),
        x if x == Tag::Float as u8 => Ok(Float(cur.read_f32()?)),
        x if x == Tag::Double as u8 => Ok(Double(cur.read_f64()?)),
        x if x == Tag::ByteArray as u8 => {
            let n = cur.read_i32()? as usize;
            let mut v = vec![0u8; n];
            v.copy_from_slice(cur.read_exact(n)?);
            Ok(ByteArray(v))
        }
        x if x == Tag::String as u8 => Ok(String(cur.read_string()?)),
        x if x == Tag::List as u8 => {
            let child = cur.read_u8()?;
            let n = cur.read_i32()? as usize;
            let mut items = Vec::with_capacity(n);
            for _ in 0..n { items.push(read_payload(cur, child)?); }
            Ok(List(child, items))
        }
        x if x == Tag::Compound as u8 => {
            let mut map = HashMap::new();
            loop {
                let t = cur.read_u8()?;
                if t == Tag::End as u8 { break; }
                let key = cur.read_string()?;
                let val = read_payload(cur, t)?;
                map.insert(key, val);
            }
            Ok(Compound(map))
        }
        x if x == Tag::IntArray as u8 => {
            let n = cur.read_i32()? as usize;
            let mut v = Vec::with_capacity(n);
            for _ in 0..n { v.push(cur.read_i32()?); }
            Ok(IntArray(v))
        }
        x if x == Tag::LongArray as u8 => {
            let n = cur.read_i32()? as usize;
            let mut v = Vec::with_capacity(n);
            for _ in 0..n { v.push(cur.read_i64()?); }
            Ok(LongArray(v))
        }
        _ => Err(io::Error::new(io::ErrorKind::InvalidData, "unknown tag")),
    }
}

fn parse_root_nbt(data: &[u8]) -> io::Result<Nbt> {
    let mut c = Cursor::new(data);
    let tid = c.read_u8()?; // root id (usually Compound)
    if tid == Tag::End as u8 {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "empty nbt"));
    }
    let _root_name = c.read_string()?;
    read_payload(&mut c, tid)
}

fn nbt_get_compound<'a>(nbt: &'a Nbt) -> Option<&'a HashMap<String, Nbt>> {
    if let Nbt::Compound(m) = nbt { Some(m) } else { None }
}
fn nbt_get_list<'a>(nbt: &'a Nbt) -> Option<(u8, &'a Vec<Nbt>)> {
    if let Nbt::List(t, v) = nbt { Some((*t, v)) } else { None }
}
fn nbt_get_i32(nbt: &Nbt) -> Option<i32> { if let Nbt::Int(v) = nbt { Some(*v) } else { None } }
fn nbt_get_str<'a>(nbt: &'a Nbt) -> Option<&'a str> { if let Nbt::String(s) = nbt { Some(s.as_str()) } else { None } }
fn nbt_get_bytes<'a>(nbt: &'a Nbt) -> Option<&'a [u8]> { if let Nbt::ByteArray(b) = nbt { Some(b.as_slice()) } else { None } }

fn nbt_get_i8_as_i32(n: &Nbt) -> Option<i32> {
    match n {
        Nbt::Byte(b) => Some(*b as i32),
        Nbt::Int(i) => Some(*i),
        Nbt::Short(s) => Some(*s as i32),
        _ => None,
    }
}

/// Blocks section: returns (Blocks low8, Add nibbles)
fn section_blocks(section: &HashMap<String, Nbt>) -> Option<(&[u8], Option<&[u8]>)> {
    let blocks = section.get("Blocks").and_then(nbt_get_bytes)?;
    let add = section.get("Add").and_then(nbt_get_bytes);
    Some((blocks, add))
}

#[inline]
fn block_id_at(blocks: &[u8], add: Option<&[u8]>, idx: usize) -> i32 {
    let low = blocks[idx] as i32;
    if let Some(addb) = add {
        let nib = addb[idx / 2];
        let upper = if idx % 2 == 0 { nib & 0x0F } else { (nib >> 4) & 0x0F };
        ((upper as i32) << 8) | low
    } else {
        low
    }
}

/// ---------------------------
/// Scan logic
/// ---------------------------
#[derive(Clone, Copy, Debug)]
struct Pt { x: i32, y: i32, z: i32 }

fn find_blocks_in_chunk(level: &HashMap<String, Nbt>, targets: &HashSet<i32>, y_min: i32, y_max: i32) -> Vec<Pt> {
    let mut out = Vec::new();
    let sections = match level.get("Sections") {
        Some(n) => n,
        None => return out,
    };
    let (_, list) = match nbt_get_list(sections) {
        Some(x) => x,
        None => return out,
    };

    let cx = level.get("xPos").and_then(nbt_get_i32).unwrap_or(0);
    let cz = level.get("zPos").and_then(nbt_get_i32).unwrap_or(0);

    for sec in list {
        let sec_comp = match nbt_get_compound(sec) { Some(c) => c, None => continue };
        let y_idx = sec_comp.get("Y").and_then(nbt_get_i8_as_i32).unwrap_or(0);
        let sec_y_min = y_idx * 16;
        let sec_y_max = sec_y_min + 15;
        if sec_y_max < y_min || sec_y_min > y_max { continue; }

        let (blocks, add) = match section_blocks(sec_comp) { Some(x) => x, None => continue };
        // index = y*256 + z*16 + x  (x,z,y in 0..15)
        for y_off in 0..16 {
            let ay = sec_y_min + y_off;
            if ay < y_min || ay > y_max { continue; }
            let base_y = y_off * 256;
            for z in 0..16 {
                let base_zy = base_y + z * 16;
                for x in 0..16 {
                    let idx = (base_zy + x) as usize;
                    let bid = block_id_at(blocks, add, idx);
                    if targets.contains(&bid) {
                        let wx = cx * 16 + x;
                        let wz = cz * 16 + z;
                        out.push(Pt { x: wx, y: ay, z: wz });
                    }
                }
            }
        }
    }
    out
}

fn find_tes_in_chunk(level: &HashMap<String, Nbt>, wanted: &HashSet<String>) -> Vec<Pt> {
    let mut out = Vec::new();
    let tes = match level.get("TileEntities") {
        Some(n) => n,
        None => return out,
    };
    let (_, list) = match nbt_get_list(tes) { Some(x) => x, None => return out };
    for te in list {
        let comp = match nbt_get_compound(te) { Some(c) => c, None => continue };
        let id = comp.get("id").and_then(nbt_get_str).unwrap_or("");
        if wanted.contains(id) {
            let x = comp.get("x").and_then(nbt_get_i32).unwrap_or(0);
            let y = comp.get("y").and_then(nbt_get_i32).unwrap_or(64);
            let z = comp.get("z").and_then(nbt_get_i32).unwrap_or(0);
            out.push(Pt { x, y, z });
        }
    }
    out
}

/// ---------------------------
/// Waypoints
/// ---------------------------
#[derive(Serialize)]
struct Waypoint {
    id: String,
    name: String,
    x: i32, y: i32, z: i32,
    enabled: bool,
    icon: String,
    r: u8, g: u8, b: u8,
    dim: i32,
}

fn load_existing_waypoints(dir: &Path, dim: i32) -> io::Result<Vec<Pt>> {
    let mut pts = Vec::new();
    if !dir.is_dir() { return Ok(pts); }
    for e in fs::read_dir(dir)? {
        let p = e?.path();
        if p.extension().map(|s| s == "json").unwrap_or(false) {
            if let Ok(txt) = fs::read_to_string(&p) {
                if let Ok(v) = serde_json::from_str::<serde_json::Value>(&txt) {
                    let dm = v.get("dim").and_then(|x| x.as_i64()).unwrap_or(0) as i32;
                    if dm != dim { continue; }
                    let x = v.get("x").and_then(|x| x.as_i64()).unwrap_or(0) as i32;
                    let y = v.get("y").and_then(|x| x.as_i64()).unwrap_or(64) as i32;
                    let z = v.get("z").and_then(|x| x.as_i64()).unwrap_or(0) as i32;
                    pts.push(Pt { x, y, z });
                }
            }
        }
    }
    Ok(pts)
}

fn nearest_dist2(p: Pt, set: &[Pt]) -> Option<i64> {
    let mut best: Option<i64> = None;
    for q in set {
        let dx = (q.x - p.x) as i64;
        let dy = (q.y - p.y) as i64;
        let dz = (q.z - p.z) as i64;
        let d2 = dx*dx + dy*dy + dz*dz;
        if best.map_or(true, |b| d2 < b) { best = Some(d2); }
    }
    best
}

fn write_waypoint(dir: &Path, dim: i32, wid: &str, name: &str, p: Pt, icon: &str, rgb: (u8,u8,u8)) -> io::Result<PathBuf> {
    fs::create_dir_all(dir)?;
    let wp = Waypoint {
        id: wid.to_string(),
        name: name.to_string(),
        x: p.x, y: p.y, z: p.z,
        enabled: true,
        icon: icon.to_string(),
        r: rgb.0, g: rgb.1, b: rgb.2,
        dim,
    };
    let path = dir.join(format!("{wid}.json"));
    let mut f = File::create(&path)?;
    let s = serde_json::to_string_pretty(&wp)?;
    f.write_all(s.as_bytes())?;
    Ok(path)
}

/// ---------------------------
/// Clustering (simple greedy)
/// ---------------------------
fn cluster_points(mut pts: Vec<Pt>, radius: i32) -> Vec<Pt> {
    if radius <= 0 || pts.is_empty() { return pts; }
    let r2 = (radius as i64) * (radius as i64);
    let mut out: Vec<Pt> = Vec::new();
    let mut used = vec![false; pts.len()];
    for i in 0..pts.len() {
        if used[i] { continue; }
        let mut cx = pts[i].x as i64;
        let mut cy = pts[i].y as i64;
        let mut cz = pts[i].z as i64;
        let mut count = 1i64;
        used[i] = true;
        for j in (i+1)..pts.len() {
            if used[j] { continue; }
            let dx = (pts[j].x - pts[i].x) as i64;
            let dy = (pts[j].y - pts[i].y) as i64;
            let dz = (pts[j].z - pts[i].z) as i64;
            if dx*dx + dy*dy + dz*dz <= r2 {
                used[j] = true;
                cx += pts[j].x as i64; cy += pts[j].y as i64; cz += pts[j].z as i64;
                count += 1;
            }
        }
        out.push(Pt { x: (cx / count) as i32, y: (cy / count) as i32, z: (cz / count) as i32 });
    }
    out
}

/// ---------------------------
/// Parse helpers
/// ---------------------------
fn parse_ids(s: &str) -> io::Result<HashSet<i32>> {
    let mut out = HashSet::new();
    for raw in s.split(',') {
        let t = raw.trim();
        if t.is_empty() { continue; }
        let v = if let Some(stripped) = t.strip_prefix("0x") {
            i32::from_str_radix(stripped, 16).map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "bad hex id"))?
        } else {
            t.parse::<i32>().map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "bad id"))?
        };
        out.insert(v);
    }
    Ok(out)
}
fn parse_te_ids(s: &str) -> HashSet<String> {
    s.split(',').map(|t| t.trim().to_string()).filter(|s| !s.is_empty()).collect()
}
fn parse_rgb(s: &str) -> io::Result<(u8,u8,u8)> {
    let parts: Vec<_> = s.split(',').collect();
    if parts.len() != 3 { return Err(io::Error::new(io::ErrorKind::InvalidInput, "color must be R,G,B")); }
    let r = parts[0].trim().parse::<u8>().map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "bad R"))?;
    let g = parts[1].trim().parse::<u8>().map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "bad G"))?;
    let b = parts[2].trim().parse::<u8>().map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "bad B"))?;
    Ok((r,g,b))
}

/// ---------------------------
/// Main
/// ---------------------------
fn main() -> io::Result<()> {
    let cli = Cli::parse();

    let targets = match &cli.ids {
        Some(s) => Some(parse_ids(s)?),
        None => None,
    };
    let te_targets = match &cli.te_ids {
        Some(s) => Some(parse_te_ids(s)),
        None => None,
    };
    let rgb = parse_rgb(&cli.color)?;

    // List regions
    let regions = list_region_files(&cli.world, cli.dim)?;
    if regions.is_empty() {
        eprintln!("No region files found for dim {} under {:?}", cli.dim, cli.world);
        std::process::exit(3);
    }

    // Load existing waypoints only if we might write
    let mut existing: Vec<Pt> = if cli.write {
        if cli.jm_waypoints.is_none() {
            eprintln!("--write requires --jm-waypoints <dir>");
            std::process::exit(2);
        }
        load_existing_waypoints(cli.jm_waypoints.as_ref().unwrap(), cli.dim)?
    } else {
        Vec::new()
    };

    println!(
        "Regions: {} | Mode: {}",
        regions.len(),
        if cli.write { "WRITE" } else { "PREVIEW" }
    );
    if cli.write {
        println!("Existing waypoints in dim {}: {}", cli.dim, existing.len());
    }

    let start = Instant::now();
    let mut hits: Vec<Pt> = Vec::new();

    for rp in regions {
        let mut reg = RegionFile::open(&rp)?;
        for cx in 0..32 {
            for cz in 0..32 {
                let raw = match reg.read_chunk(cx, cz)? {
                    Some(v) => v,
                    None => continue,
                };
                let root = match parse_root_nbt(&raw) {
                    Ok(n) => n,
                    Err(_) => continue,
                };
                let level = match nbt_get_compound(&root).and_then(|m| m.get("Level")).and_then(nbt_get_compound) {
                    Some(lv) => lv,
                    None => continue,
                };

                if let Some(t) = &targets {
                    hits.extend(find_blocks_in_chunk(level, t, cli.y_min, cli.y_max));
                }
                if let Some(tes) = &te_targets {
                    hits.extend(find_tes_in_chunk(level, tes));
                }
            }
        }
    }

    println!("Raw hits: {}", hits.len());
    let mut points = if cli.cluster_radius > 0 {
        let clustered = cluster_points(hits, cli.cluster_radius);
        println!("After clustering @ {}: {}", cli.cluster_radius, clustered.len());
        clustered
    } else {
        hits
    };

    // PREVIEW MODE (default)
    if !cli.write {
        for p in &points {
            println!("HIT: x={} y={} z={}", p.x, p.y, p.z);
        }
        println!("Preview complete. Re-run with --write --jm-waypoints <dir> to create waypoints.");
        return Ok(());
    }

    // WRITE MODE
    let jm_dir = cli.jm_waypoints.as_ref().unwrap();
    fs::create_dir_all(jm_dir)?;
    let thresh2 = (cli.near_threshold as i64) * (cli.near_threshold as i64);
    let label = cli.label.clone().unwrap_or_else(|| {
        match (&cli.ids, &cli.te_ids) {
            (Some(_), Some(_)) => "Found TE/Blocks".to_string(),
            (Some(_), None) => "Found Block".to_string(),
            (None, Some(_)) => "Found TE".to_string(),
            _ => "Found".to_string(),
        }
    });

    let mut added = 0usize;
    let mut skipped = 0usize;

    for p in points.drain(..) {
        if let Some(d2) = nearest_dist2(p, &existing) {
            if d2 <= thresh2 { skipped += 1; continue; }
        }
        let wid = format!("F_{}_{}_{}_{}", cli.dim, p.x, p.y, p.z);
        let name = format!("{} ({}, {})", label, p.x, p.z);
        let path = write_waypoint(jm_dir, cli.dim, &wid, &name, p, &cli.icon, rgb)?;
        println!("+ {}", path.display());
        existing.push(p);
        added += 1;
    }

    println!("Added {} new waypoints; skipped {}. (Elapsed: {:?})", added, skipped, start.elapsed());
    Ok(())
}
