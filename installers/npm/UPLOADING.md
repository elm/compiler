## Uploading new binaries

1. Assemble a `.tar.gz` file for each desired operating system/architecture combination desired. Each tarball should contain a directory (e.g. `Elm-Platform-0.15`, although the directory name does not matter) whose contents are the binaries like `elm-make` for the appropriate environment. The tarballs must benamed according to this format: `(`[`process.platform`](https://nodejs.org/api/process.html#process_process_platform)` + "-" + `[`process.arch`](https://nodejs.org/api/process.html#process_process_arch)` + ".tar.gz")` - e.g. for 64-bit OS X, `darwin-x64.tar.gz`.
2. Sign into [the elm-platform npm binaries repo](https://bintray.com/elmlang/elm-platform/npm), select the appropriate version, and click Upload
3. Enter the version number (e.g. `0.15.1`) under **Target Repository Path**.
4. Click **Upload Files** and select the `.tar.gz` files from earlier. (You can multi-select in the dialog.)
5. Once everything is done uploading, hit **Save Changes**. You should now see each of the files listed at the bottom of the page, along with an auto-generated `.asc` file which contains a PGP signature so people can verify the files were downloaded correctly.
6. If everything looks right, hit **Publish** to make them available for download.
