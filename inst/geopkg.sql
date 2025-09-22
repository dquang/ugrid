
CREATE TABLE IF NOT EXISTS gpkg_extensions (
  table_name TEXT,
  column_name TEXT,
  extension_name TEXT NOT NULL,
  definition TEXT NOT NULL,
  scope TEXT NOT NULL,
  CONSTRAINT ge_tce UNIQUE (table_name, column_name, extension_name)
);

INSERT OR REPLACE INTO gpkg_extensions
        (extension_name, definition, scope)
        VALUES ("gpkg_metadata", "http://www.geopackage.org/spec/#extension_metadata", "read-write")
        ;

CREATE TABLE IF NOT EXISTS gpkg_metadata (
    id INTEGER CONSTRAINT m_pk PRIMARY KEY ASC NOT NULL,
    md_scope TEXT NOT NULL DEFAULT 'dataset',
    md_standard_uri TEXT NOT NULL DEFAULT 'http://www.isotc211.org/2005/gmd',
    mime_type TEXT NOT NULL DEFAULT 'text/xml',
    metadata TEXT NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS gpkg_metadata_reference (
    reference_scope TEXT NOT NULL,
    table_name TEXT,
    column_name TEXT,
    row_id_value INTEGER,
    timestamp DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
    md_file_id INTEGER NOT NULL,
    md_parent_id INTEGER,
    CONSTRAINT crmr_mfi_fk FOREIGN KEY (md_file_id) REFERENCES gpkg_metadata(id),
    CONSTRAINT crmr_mpi_fk FOREIGN KEY (md_parent_id) REFERENCES gpkg_metadata(id)
);

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_md_scope_insert'
BEFORE INSERT ON 'gpkg_metadata'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'insert on table gpkg_metadata violates
constraint: md_scope must be one of undefined | fieldSession |
collectionSession | series | dataset | featureType | feature |
attributeType | attribute | tile | model | catalog | schema |
taxonomy software | service | collectionHardware |
nonGeographicDataset | dimensionGroup')
WHERE NOT(NEW.md_scope IN
          ('undefined','fieldSession','collectionSession','series','dataset',
              'featureType','feature','attributeType','attribute','tile','model',
              'catalog','schema','taxonomy','software','service',
              'collectionHardware','nonGeographicDataset','dimensionGroup'));
END;

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_md_scope_update'
BEFORE UPDATE OF 'md_scope' ON 'gpkg_metadata'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'update on table gpkg_metadata violates constraint: md_scope must be one of undefined | fieldSession | collectionSession | series | dataset | featureType | feature | attributeType | attribute | tile | model | catalog | schema | taxonomy software | service | collectionHardware | nonGeographicDataset | dimensionGroup')
WHERE NOT(NEW.md_scope IN
          ('undefined','fieldSession','collectionSession','series','dataset',
              'featureType','feature','attributeType','attribute','tile','model',
              'catalog','schema','taxonomy','software','service',
              'collectionHardware','nonGeographicDataset','dimensionGroup'));
END;

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_reference_reference_scope_insert'
BEFORE INSERT ON 'gpkg_metadata_reference'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'insert on table gpkg_metadata_reference violates constraint: reference_scope must be one of "geopackage", table", "column", "row", "row/col"')
WHERE NOT NEW.reference_scope IN
('geopackage','table','column','row','row/col');
END;

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_reference_reference_scope_update'
BEFORE UPDATE OF 'reference_scope' ON 'gpkg_metadata_reference'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'update on table gpkg_metadata_reference violates constraint: referrence_scope must be one of "geopackage", "table", "column", "row", "row/col"')
WHERE NOT NEW.reference_scope IN
('geopackage','table','column','row','row/col');
END;

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_reference_column_name_insert'
BEFORE INSERT ON 'gpkg_metadata_reference'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'insert on table gpkg_metadata_reference violates constraint: column name must be NULL when reference_scope is "geopackage", "table" or "row"')
WHERE (NEW.reference_scope IN ('geopackage','table','row')
       AND NEW.column_name IS NOT NULL);
SELECT RAISE(ABORT, 'insert on table gpkg_metadata_reference
violates constraint: column name must be defined for the specified
table when reference_scope is "column" or "row/col"')
WHERE (NEW.reference_scope IN ('column','row/col')
       AND NOT NEW.table_name IN (
           SELECT name FROM SQLITE_MASTER WHERE type = 'table'
           AND name = NEW.table_name
           AND sql LIKE ('%' || NEW.column_name || '%')));
END;

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_reference_column_name_update'
BEFORE UPDATE OF column_name ON 'gpkg_metadata_reference'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'update on table gpkg_metadata_reference violates constraint: column name must be NULL when reference_scope is "geopackage", "table" or "row"')
WHERE (NEW.reference_scope IN ('geopackage','table','row')
       AND NEW.column_nameIS NOT NULL);
SELECT RAISE(ABORT, 'update on table gpkg_metadata_reference
violates constraint: column name must be defined for the specified
table when reference_scope is "column" or "row/col"')
WHERE (NEW.reference_scope IN ('column','row/col')
       AND NOT NEW.table_name IN (
           SELECT name FROM SQLITE_MASTER WHERE type = 'table'
           AND name = NEW.table_name
           AND sql LIKE ('%' || NEW.column_name || '%')));
END;

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_reference_row_id_value_insert'
BEFORE INSERT ON 'gpkg_metadata_reference'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'insert on table gpkg_metadata_reference violates constraint: row_id_value must be NULL when reference_scope is "geopackage", "table" or "column"')
WHERE NEW.reference_scope IN ('geopackage','table','column')
AND NEW.row_id_value IS NOT NULL;
END;

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_reference_row_id_value_update'
BEFORE UPDATE OF 'row_id_value' ON 'gpkg_metadata_reference'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'update on table gpkg_metadata_reference violates constraint: row_id_value must be NULL when reference_scope is "geopackage", "table" or "column"')
WHERE NEW.reference_scope IN ('geopackage','table','column')
AND NEW.row_id_value IS NOT NULL;
END;

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_reference_timestamp_insert'
BEFORE INSERT ON 'gpkg_metadata_reference'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'insert on table gpkg_metadata_reference violates constraint: timestamp must be a valid time in ISO 8601 "yyyy-mm-ddThh:mm:ss.cccZ" form')
WHERE NOT (NEW.timestamp GLOB
           '[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]T[0-2][0-9]:[0-5][0-9]:[0-5][0-9].[0-9][0-9][0-9]Z'
           AND strftime('%s',NEW.timestamp) NOT NULL);
END;

CREATE TRIGGER IF NOT EXISTS 'gpkg_metadata_reference_timestamp_update'
BEFORE UPDATE OF 'timestamp' ON 'gpkg_metadata_reference'
FOR EACH ROW BEGIN
SELECT RAISE(ABORT, 'update on table gpkg_metadata_reference violates constraint: timestamp must be a valid time in ISO 8601 "yyyy-mm-ddThh:mm:ss.cccZ" form')
WHERE NOT (NEW.timestamp GLOB
           '[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]T[0-2][0-9]:[0-5][0-9]:[0-5][0-9].[0-9][0-9][0-9]Z'
           AND strftime('%s',NEW.timestamp) NOT NULL);
END;
