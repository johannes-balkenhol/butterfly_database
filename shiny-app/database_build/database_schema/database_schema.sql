-- Specimen Table
CREATE TABLE specimens (
    PID TEXT PRIMARY KEY,
    suborder TEXT,
    family TEXT,
    genus TEXT,
    species TEXT,
    genus_and_species TEXT,
    german_name TEXT,
    location TEXT,
    land TEXT,
    lat REAL,
    lon REAL,
    elevation REAL,
    D_M_Y DATE,
    date_accuracy TEXT,
    total_number_of_this_species INTEGER,
    FFH_directive TEXT,
    red_list_Bavaria TEXT,
    breeding TEXT,
    collector TEXT,
    notes_of_the_digitizer TEXT,
    pictures TEXT
);

-- Species Traits
CREATE TABLE species_traits (
    species_id INTEGER PRIMARY KEY,
    genus TEXT,
    species TEXT,
    ncbi_tax_id TEXT,
    accepted_name TEXT,
    eol_id TEXT,
    eol_links TEXT
);

-- Environmental Traits
CREATE TABLE location_traits (
    location_id INTEGER PRIMARY KEY,
    location TEXT,
    land TEXT,
    lat REAL,
    lon REAL,
    elevation REAL,
    annual_temp REAL,
    annual_precip REAL
);

-- Color Brightness Data
CREATE TABLE color_brightness (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    genus_and_species TEXT,
    location TEXT,
    elevation REAL,
    color_brightness REAL
);
