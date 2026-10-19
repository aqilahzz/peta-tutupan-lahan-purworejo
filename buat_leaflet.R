# ============================================================
# WEBGIS LEAFLET - KLASIFIKASI TUTUPAN LAHAN PURWOREJO
# Jalankan dari folder project ini di RStudio.
# ============================================================

# Instal sekali jika package belum tersedia:
# install.packages(c("terra", "sf", "leaflet", "htmlwidgets", "htmltools"))

library(terra)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# --- 1. Lokasi file (gunakan path relatif agar bisa dipakai di komputer lain)
file_raster <- file.path("data", "klasifikasi_purworejo.tif")
file_kab <- file.path("data", "Batas Adm Kabupaten_Purworejo.shp")
file_kec <- file.path("data", "Batas Adm Kecamatan_Purworejo.shp")
folder_docs <- "docs"

dir.create(folder_docs, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(file_raster)) {
  stop("Raster tidak ditemukan: ", file_raster,
       "\nSalin klasifikasi_purworejo.tif hasil pengolahan R kamu ke folder data/.")
}
if (!file.exists(file_kab)) {
  stop("Shapefile kabupaten tidak ditemukan: ", file_kab,
       "\nSalin .shp beserta file pendampingnya (.shx, .dbf, .prj, dll.) ke folder data/.")
}
if (!file.exists(file_kec)) {
  stop("Shapefile kecamatan tidak ditemukan: ", file_kec,
       "\nSalin .shp beserta file pendampingnya (.shx, .dbf, .prj, dll.) ke folder data/.")
}

# --- 2. Baca data
r <- terra::rast(file_raster)
kab <- sf::st_read(file_kab, quiet = TRUE)
kec <- sf::st_read(file_kec, quiet = TRUE)

if (is.na(terra::crs(r))) stop("CRS raster tidak diketahui.")
if (is.na(sf::st_crs(kab))) stop("CRS shapefile kabupaten tidak diketahui.")
if (is.na(sf::st_crs(kec))) stop("CRS shapefile kecamatan tidak diketahui.")

# Pastikan raster punya satu layer klasifikasi
if (terra::nlyr(r) > 1) r <- r[[1]]

# Raster 10 m bisa terlalu berat untuk web. Agregasi menjadi sekitar 50 m
# agar HTML lebih ringan; nilai kelas dipertahankan menggunakan modal.
faktor <- 5
r_web <- terra::aggregate(r, fact = faktor, fun = modal, na.rm = TRUE)

# Leaflet menggunakan koordinat geografis WGS 84.
r_web <- terra::project(r_web, "EPSG:4326", method = "near")
kab <- sf::st_transform(kab, 4326)
kec <- sf::st_transform(kec, 4326)

# --- 3. Warna dan label kelas
warna_kelas <- c(
  "1" = "#329447", # Vegetasi
  "2" = "#D96050", # Bangunan
  "3" = "#3589CD", # Perairan
  "4" = "#D9D9D9"  # Awan
)
label_kelas <- c("1" = "Vegetasi", "2" = "Bangunan",
                 "3" = "Perairan", "4" = "Awan")

# Buat palet raster berdasarkan nilai kelas
pal <- leaflet::colorFactor(
  palette = unname(warna_kelas),
  levels = c(1, 2, 3, 4),
  na.color = "transparent"
)

# Kolom tooltip kecamatan: gunakan kolom nama yang paling mungkin tersedia.
kolom_nama <- grep("nama|kecamatan|WADMKC|NAMOBJ", names(kec),
                   ignore.case = TRUE, value = TRUE)
if (length(kolom_nama) > 0) {
  kec$NamaKecamatanWeb <- as.character(kec[[kolom_nama[1]]])
} else {
  kec$NamaKecamatanWeb <- paste("Kecamatan", seq_len(nrow(kec)))
}

# --- 4. Bangun peta interaktif
peta <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>
  addRasterImage(
    r_web,
    colors = pal,
    opacity = 0.72,
    project = FALSE,
    group = "Klasifikasi tutupan lahan",
    maxBytes = 12 * 1024 * 1024
  ) |>
  addPolygons(
    data = kec,
    fill = FALSE,
    color = "#555555",
    weight = 1,
    opacity = 0.8,
    group = "Batas kecamatan",
    label = ~NamaKecamatanWeb,
    highlightOptions = highlightOptions(
      weight = 2, color = "#222222", bringToFront = TRUE
    )
  ) |>
  addPolygons(
    data = kab,
    fill = FALSE,
    color = "#111111",
    weight = 2,
    opacity = 1,
    group = "Batas kabupaten"
  ) |>
  addLegend(
    position = "bottomright",
    colors = unname(warna_kelas),
    labels = unname(label_kelas),
    title = "Tutupan Lahan",
    opacity = 1
  ) |>
  addLayersControl(
    overlayGroups = c("Klasifikasi tutupan lahan",
                      "Batas kecamatan", "Batas kabupaten"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  fitBounds(
    lng1 = sf::st_bbox(kab)["xmin"],
    lat1 = sf::st_bbox(kab)["ymin"],
    lng2 = sf::st_bbox(kab)["xmax"],
    lat2 = sf::st_bbox(kab)["ymax"]
  )

# --- 5. Simpan HTML dan dependency ke folder docs
htmlwidgets::saveWidget(
  widget = peta,
  file = file.path(folder_docs, "index.html"),
  selfcontained = FALSE,
  title = "WebGIS Klasifikasi Tutupan Lahan Kabupaten Purworejo"
)

# Salin README singkat ke folder docs sebagai halaman pendamping (opsional)
message("Berhasil membuat docs/index.html")
message("Untuk publikasi: push folder docs/ ke GitHub, lalu aktifkan Pages dari branch main / folder docs.")
