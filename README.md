# gtnh_finder

Offline block / TileEntity scanner for Minecraft 1.7.10 worlds.\
Finds matching blocks or TileEntities inside region (`.mca`) files and
optionally creates [JourneyMap
5.x](https://www.curseforge.com/minecraft/mc-mods/journeymap) waypoints
for them.

------------------------------------------------------------------------

## Features

-   Scan **blocks by numeric ID** (decimal or hex).
-   Scan **TileEntities by ID** (e.g. `Chest`, `MobSpawner`).
-   Restrict search by **dimension** and **Y range**.
-   **Cluster hits** into a single waypoint (e.g. for dungeons).
-   **Skip duplicates** near existing waypoints.
-   **Preview mode** (default) to print hits without writing.
-   **Write mode** to export waypoints into JourneyMap.

------------------------------------------------------------------------

## Installation

### Recommended: Download Release

1.  Go to the [Releases
    page](https://github.com/Kerpackie/gtnh_finder/releases).
2.  Download the prebuilt binary for your OS (`gtnh_finder.exe` for
    Windows, or the Linux binary).
3.  Place it somewhere in your PATH, or next to your Minecraft world.

### Alternative: Build from Source

If you prefer compiling yourself:

``` bash
git clone https://github.com/yourname/gtnh_finder.git
cd gtnh_finder
cargo build --release
```

The binary will be at `target/release/gtnh_finder`.

------------------------------------------------------------------------

## Usage

### Basic CLI

``` bash
gtnh_finder --world <WORLD_SAVE_DIR> --ids <BLOCK_IDS>
```

``` bash
gtnh_finder --world <WORLD_SAVE_DIR> --te-ids <TE_IDS>
```

### Options

  -----------------------------------------------------------------------
  Option                      Description
  --------------------------- -------------------------------------------
  `--world <dir>`             Path to Minecraft world folder (contains
                              `region/`, `DIM-1/region/`, etc.).

  `--jm-waypoints <dir>`      Path to JourneyMap 5.x `waypoints` folder.
                              Required only when writing waypoints.

  `--dim <id>`                Dimension (0=Overworld, -1=Nether, 1=End).
                              Default: 0

  `--ids <list>`              Comma-separated block IDs
                              (e.g. `3460,3459,0xD84`).

  `--te-ids <list>`           Comma-separated TileEntity IDs
                              (e.g. `Chest,MobSpawner`).

  `--y-min <n>` /             Y scan range. Default: 0--255.
  `--y-max <n>`               

  `--near-threshold <n>`      Skip writing waypoint if one already exists
                              within N blocks. Default: 10

  `--cluster-radius <n>`      Merge hits within radius into a single
                              waypoint. Default: 0 (disabled).

  `--icon <name>`             Waypoint icon. Default: `temple`.

  `--label <str>`             Waypoint label. Default depends on search
                              type.

  `--color <R,G,B>`           RGB color. Default: `255,255,255`.

  `--write`                   Enable write mode (otherwise preview only).
  
  -----------------------------------------------------------------------

### Examples

-   **Preview all spawners in Nether (`DIM-1`):**

    ``` bash
    gtnh_finder --world ~/minecraft/saves/MyWorld --dim -1 --te-ids MobSpawner
    ```

-   **Write waypoints for block ID `3460` in Overworld:**

    ``` bash
    gtnh_finder --world ~/minecraft/saves/MyWorld --ids 3460 --write --jm-waypoints ~/.minecraft/journeymap/data/mp/MyServer/waypoints
    ```

-   **Cluster spawners within 32 blocks:**

    ``` bash
    gtnh_finder --world ./saves/GTNH --dim -1 --te-ids MobSpawner --cluster-radius 32 --write --jm-waypoints ./journeymap/waypoints
    ```
### Windows Examples

-   **Preview all Mob Spawners (no files written):**

    ``` bash
    gtnh_finder.exe --world %APPDATA%\.minecraft\saves\MyWorld" --dim -1 --te-ids MobSpawner
    ```

-   **Preview all Mob Spawners (no files written):**

    ``` bash
    gtnh_finder.exe --world %APPDATA%\.minecraft\saves\MyWorld" --dim 0 --ids 3460 --y-min 20 --y-max 64 --write --jm-waypoints "%APPDATA%\.minecraft\journeymap\data\sp\MyWorld\waypoints"

    ```

-   **Preview all Mob Spawners (no files written):**

    ``` bash
    gtnh_finder.exe --world %APPDATA%\.minecraft\saves\MyWorld" --dim 0 --te-ids MobSpawner --cluster-radius 64 --write --jm-waypoints "%APPDATA%\.minecraft\journeymap\data\sp\MyWorld\waypoints"
    ```

------------------------------------------------------------------------

## Notes

-   Tested with **Minecraft 1.7.10** and **JourneyMap 5.x**.
-   Large worlds may take several minutes to scan.
-   Uses a minimal custom NBT parser (no external deps beyond `flate2`,
    `serde`, `clap`).

