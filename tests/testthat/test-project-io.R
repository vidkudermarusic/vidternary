# Tests for R/project_io.R: one file with the settings of all tabs, and
# for the parts of R/settings_io.R it builds on.
library(shiny)

upload_value <- function(names) data.frame(name = names, size = 1, type = "", datapath = paste0("/tmp/", names), stringsAsFactors = FALSE)
as_upload <- function(path) data.frame(name = basename(path), size = 1, type = "", datapath = path, stringsAsFactors = FALSE)

test_that("the names of the loaded files are collected per upload box", {
  values <- list(`evs-evs_files` = upload_value("a.xlsx"), `batch-batch_files` = upload_value(c("b1.csv", "b2.csv")),
                 `evs-evs_settings_load` = upload_value("old.json"), `import-import_profile_load` = upload_value("p.json"),
                 `evs-evs_area_col` = "Area", `coda-table` = data.frame(x = 1))
  expect_equal(uploaded_file_names(values), list(`batch-batch_files` = c("b1.csv", "b2.csv"), `evs-evs_files` = "a.xlsx"))
  expect_equal(uploaded_file_names(list(a = 1)), stats::setNames(list(), character(0)))
})

test_that("a settings file can carry a kind and further entries", {
  path <- tempfile(fileext = ".json")
  write_settings_file(list(`evs-evs_T` = 380, `coda-coda_parts` = c("Al", "Si")), "project", path, kind = "project",
                      extra = list(files = list(`evs-evs_files` = "a.xlsx", `batch-batch_files` = c("b1.csv", "b2.csv"))))
  back <- read_settings_file(path, "project", kind = "project")
  expect_equal(back$settings, list(`evs-evs_T` = 380, `coda-coda_parts` = c("Al", "Si")))
  expect_equal(back$extra$files$`evs-evs_files`, "a.xlsx")
  expect_equal(back$extra$files$`batch-batch_files`, c("b1.csv", "b2.csv"))
  # each kind of file is refused where the other belongs, with a pointer
  expect_error(read_settings_file(path, "evs"), "This is a project file: load it with \"Load project\"", fixed = TRUE)
  tab_file <- tempfile(fileext = ".json"); write_settings_file(list(a = 1), "evs", tab_file)
  expect_error(read_settings_file(tab_file, "project", kind = "project"), "holds the settings of the 'evs' tab: load it in that tab")
  other <- tempfile(fileext = ".json"); writeLines('{"kind": "import profile"}', other)
  expect_error(read_settings_file(other, "project", kind = "project"), "not a vidternary project file")
  expect_equal(read_settings_file(tab_file, "evs")$extra, stats::setNames(list(), character(0)))
})

test_that("the status after loading names the tabs and files still waited for", {
  loaded <- list(saved = "2026-11-12 10:00:00",
                 settings = list(`evs-evs_T` = 380, `evs-evs_area_col` = "Area", `coda-coda_parts` = c("Al", "Si"), `batch-batch_n_perm` = 999),
                 extra = list(files = list(`evs-evs_files` = "a.xlsx", `coda-coda_files` = c("c1.xlsx", "c2.xlsx"))))
  expect_equal(describe_project_load(loaded), "Project saved on 2026-11-12 10:00:00: 4 settings in 3 tabs. Setting the inputs ...")
  expect_equal(describe_project_load(loaded, character(0)), "Project saved on 2026-11-12 10:00:00: 4 settings in 3 tabs. All set.")
  waiting <- describe_project_load(loaded, c("evs-evs_area_col", "coda-coda_parts"))
  expect_match(waiting, "2 set. Waiting in: coda (1; load c1.xlsx, c2.xlsx); evs (1; load a.xlsx).", fixed = TRUE)
  # a file that is loaded by now is no longer asked for
  now <- describe_project_load(loaded, c("evs-evs_area_col", "coda-coda_parts"), list(`evs-evs_files` = "a.xlsx"))
  expect_match(now, "Waiting in: coda (1; load c1.xlsx, c2.xlsx); evs (1).", fixed = TRUE)
})

# The app shell with two "tabs" worth of inputs; `record$sent` collects
# what would be set in the browser.
project_server <- function(record) {
  record$sent <- list()
  function(input, output, session) {
    register_project_io(input, output, session, retry_ms = 100,
                        send = function(id, value) record$sent[[length(record$sent) + 1]] <- list(id = id, value = value))
  }
}
sent_to <- function(record, from = 1) {
  if (length(record$sent) < from) return(character(0))
  vapply(record$sent[from:length(record$sent)], function(s) s$id, character(1))
}

test_that("saving a project writes the settings of every tab and the file names", {
  testServer(project_server(new.env()), {
    session$setInputs(`evs-evs_return_period` = 380, `evs-evs_files` = upload_value("batch1.xlsx"),
                      `coda-coda_parts` = c("Al", "Si"), `evs-evs_fit` = 1, `evs-evs_settings_load` = upload_value("old.json"),
                      `data_comparison-table_rows_selected` = c(1L, 2L), `plot_builder-plot_click` = "x")
    back <- read_settings_file(output$project_settings_save, "project", kind = "project")
    expect_equal(back$settings, list(`coda-coda_parts` = c("Al", "Si"), `evs-evs_return_period` = 380))
    expect_equal(back$extra$files, list(`evs-evs_files` = "batch1.xlsx"))
  })
})

test_that("loading a project sets what it can and waits for the data files", {
  path <- tempfile(fileext = ".json")
  write_settings_file(list(`evs-evs_return_period` = 380, `evs-evs_area_col` = "Area"), "project", path, kind = "project",
                      extra = list(files = list(`evs-evs_files` = "batch1.xlsx")))
  record <- new.env()
  testServer(project_server(record), {
    session$setInputs(`evs-evs_return_period` = 100)
    session$setInputs(project_settings_load = as_upload(path))
    expect_setequal(sent_to(record), c("evs-evs_return_period", "evs-evs_area_col"))
    expect_match(output$project_settings_status, "Setting the inputs ...", fixed = TRUE)
    # the number box takes its value; the column box has no choices without the file
    session$setInputs(`evs-evs_return_period` = 380)
    for (i in 1:5) session$elapse(100)
    expect_match(output$project_settings_status, "1 set. Waiting in: evs (1; load batch1.xlsx).", fixed = TRUE)

    # the file arrives: the waiting setting is sent again, and now it takes
    n <- length(record$sent)
    session$setInputs(`evs-evs_files` = upload_value("batch1.xlsx"))
    expect_equal(sent_to(record, n + 1), "evs-evs_area_col")
    session$setInputs(`evs-evs_area_col` = "Area")
    for (i in 1:3) session$elapse(100)
    expect_match(output$project_settings_status, "All set.", fixed = TRUE)

    # further uploads send nothing more
    n <- length(record$sent)
    session$setInputs(`coda-coda_files` = upload_value("other.xlsx"))
    expect_length(record$sent, n)
  })
})

test_that("a tab's settings file is refused by the project loader", {
  path <- tempfile(fileext = ".json")
  write_settings_file(list(evs_return_period = 380), "evs", path)
  record <- new.env()
  testServer(project_server(record), {
    session$setInputs(project_settings_load = as_upload(path))
    expect_length(record$sent, 0)
    expect_match(output$project_settings_status, "Could not load the settings: This file holds the settings of the 'evs' tab", fixed = TRUE)
  })
})

test_that("the whole app saves a project from its real inputs", {
  old_wd <- getwd()
  tmp <- tempfile("project_app"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)
  testServer(function(input, output, session) create_server_logic(input, output, session), {
    session$setInputs(`cleanliness-clean_acc_per_mm2` = 250, `planning-plan_precision` = 5, `folder-folder_standard` = "ASTM E45")
    back <- read_settings_file(output$project_settings_save, "project", kind = "project")
    expect_equal(back$settings$`cleanliness-clean_acc_per_mm2`, 250)
    expect_equal(back$settings$`planning-plan_precision`, 5)
    expect_equal(back$settings$`folder-folder_standard`, "ASTM E45")
  })
})
