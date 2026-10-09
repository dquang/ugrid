#' Generate minimal metadata for a dataset
#'
#' @param fid File identifier
#' @param pid Parent identifier (i.e. Project id)
#' @param uri URI to the file
#' @param time A time stamp in POSIXct class
#' @param title Title of the dataset
#' @param name,org,position,email,phone,street,city,postcode,homepage Contact information.
#' @param crsCode CRS code.
#' @param crsCodeSpace CRS Codespace.
#' @param abstract Abstract of the dataset.
#' @param purpose Purpose of the dataset.
#' @param credits Credits for the dataset
#' @param categories Categories of the dataset.
#' @param status Status of the dataset.
#' @param keywords Keywords of the dataset.
#' @param statement Statemet of data quality of the dataset.
#' @param spType Type of spatial data.
#' @param bbox an bbox object.
#' @param moreInfo More information about the dataset.
#'
#' @returns an XML character vector encoded by `ISOMetadata` an object.
#' @export
genMeta <- function(
        fid, pid, uri=NULL, time=Sys.time(), title="Dataset",
        name=NA, org="BfG", position=NA,
        phone="+4926113060", email="posteingang@bafg.de", homepage="http://www.bafg.de",
        street="Am Mainzer Tor 1", city="Koblenz", postcode=56068,
        crsCode="", crsCodeSpace="EPSG",
        abstract=NA, purpose=NA, credits=NULL,
        categories="inlandWaters", status="onGoing",
        keywords=c("2D-Modelling"), statement=NA, spType="vector",
        bbox=NULL, moreInfo=character(0)

) {
    if (!rlang::is_installed("geometa")) {
        message("This function needs geometa package!")
        return(NULL)
    }
    md <- geometa::ISOMetadata$new()
    md$setFileIdentifier(fid)
    md$setParentIdentifier(pid)
    md$setCharacterSet("utf8")
    md$setLanguage("ger")
    md$setDateStamp(time)
    md$setMetadataStandardName("ISO 19115-3/19139")
    md$setMetadataStandardVersion("1.0")
    if (length(uri) == 1)
        md$setDataSetURI(uri)

    # Contact information---------------------------
    rp <- geometa::ISOResponsibleParty$new()
    rp$setIndividualName(name)
    rp$setOrganisationName(org)
    rp$setPositionName(position)
    rp$setRole("pointOfContact")
    contact <- geometa::ISOContact$new()
    tel <- geometa::ISOTelephone$new()
    tel$setVoice(phone)
    contact$setPhone(tel)
    address <- geometa::ISOAddress$new()
    address$setDeliveryPoint(street)
    address$setCity(city)
    address$setPostalCode(postcode)
    address$setCountry("Germany")
    address$setEmail(email)
    contact$setAddress(address)
    res <- geometa::ISOOnlineResource$new()
    res$setLinkage(homepage)
    res$setName("Homepage")
    contact$setOnlineResource(res)
    rp$setContactInfo(contact)
    md$addContact(rp)

    # Spatial representation ------------------------------------------------------------------------------------------

    #ReferenceSystem
    rs <- geometa::ISOReferenceSystem$new()
    rsId <- geometa::ISOReferenceIdentifier$new(code = crsCode, codeSpace = crsCodeSpace)
    rs$setReferenceSystemIdentifier(rsId)
    md$addReferenceSystemInfo(rs)

    #data identification
    ident <- geometa::ISODataIdentification$new()
    ident$setAbstract(abstract)
    ident$setPurpose(purpose)
    for (aC in credits)
        ident$addCredit(aC)
    ident$addStatus(status)
    ident$addLanguage("ger")
    ident$addCharacterSet("utf8")
    for (aC in categories)
        ident$addCredit(aC)

    #citation
    ct <- geometa::ISOCitation$new()
    ct$setTitle(title)
    d <- geometa::ISODate$new()
    d$setDate(time)
    d$setDateType("creation")
    ct$addDate(d)
    ident$setCitation(ct)

    #adding extent
    extent <- geometa::ISOExtent$new()
    gbbox <- geometa::ISOGeographicBoundingBox$new(minx=bbox[1], miny=bbox[2],
                                                   maxx=bbox[3], maxy=bbox[4])
    extent$addGeographicElement(gbbox)
    ident$addExtent(extent)

    #add keywords
    kwds <- geometa::ISOKeywords$new()
    for (aK in keywords)
        kwds$addKeyword(aK)
    kwds$setKeywordType("theme")
    th <- geometa::ISOCitation$new()
    th$setTitle("General")
    th$addDate(d)
    kwds$setThesaurusName(th)
    ident$addKeywords(kwds)

    #supplementalInformation
    ident$setSupplementalInformation(moreInfo)

    #spatial representation type
    ident$addSpatialRepresentationType(spType)

    md$addIdentificationInfo(ident)

    #create dataQuality object with a 'dataset' scope
    dq <- geometa::ISODataQuality$new()
    scope <- geometa::ISODataQualityScope$new()
    scope$setLevel("dataset")
    dq$setScope(scope)

    #add lineage (more example of lineages in ISOLineage documentation)
    lineage <- geometa::ISOLineage$new()
    lineage$setStatement(statement)
    dq$setLineage(lineage)

    md$addDataQualityInfo(dq)

    #XML representation of the ISOMetadata
    xml <- md$encode()
    return(as(xml, "character"))
}

insertMeta <- function(dsn, tableName, fid, pid, bbox, title=tableName,...) {

    if (!rlang::is_installed("geometa")) {
        message("This function needs geometa package!")
        return(NULL)
    }
    # Connect to the GeoPackage
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), dsn)
    sqlFile <- system.file("geopkg.sql", package = "ugrid")
    sqlTxt <- fread(sqlFile, header=FALSE, sep="\n", blank.lines.skip = FALSE)
    sqlTxt[nchar(V1) < 1, grp := .I]
    sqlTxt[nchar(V1) < 1]
    sqlTxt[, grp := grp[1], by=.(cumsum(!is.na(grp)))]
    sqlTxt[, grp := .GRP, by=grp]
    for (aG in unique(sqlTxt$grp)) {
        sta <- sqlTxt[grp == aG, paste(V1, collapse = "\n")]
        DBI::dbExecute(conn, sta)
    }
    meta <- genMeta(fid=fid, pid=pid, bbox=bbox, title=title, ...)
    DBI::dbExecute(
        conn, "INSERT INTO gpkg_metadata (md_scope, md_standard_uri, mime_type, metadata)
        VALUES (?, ?, ?, ?)",
        params = list(
            "dataset",
            "http://www.isotc211.org/2005/gmd",
            "text/xml",
            meta
              )
        )
    metaId <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid()")[[1]]
    DBI::dbExecute(
        conn, "INSERT INTO gpkg_metadata_reference
        (reference_scope, table_name, md_file_id) VALUES (?, ?, ?)",
        params = list("table", tableName, metaId)
        )
    DBI::dbDisconnect(conn)
}
