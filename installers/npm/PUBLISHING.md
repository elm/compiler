# Publishing

Here's how to update the `npm` installer.

## 0. Overview

- There is one _main npm package_ called `elm`.
- Then there is one _binary npm package_ for each platform, called for example `@elm_binaries/darwin_arm64`.

The binary packages declare which OS and CPU they are compatible with. For example:

```json
  "os": [ "darwin" ],
  "cpu": [ "arm64" ]
```

The main npm package depend on the binary packages via [optional dependencies](https://docs.npmjs.com/cli/v9/configuring-npm/package-json#optionaldependencies):

```json
    "@elm_binaries/darwin_arm64": "0.19.1-0",
    "@elm_binaries/darwin_x64": "0.19.1-0",
    "@elm_binaries/linux_arm64": "0.19.1-0",
    ...
```

When installing, `npm` fetches the metadata for all the optional dependencies and only installs the one with a matching OS and CPU. If none of them match, `npm` still considers the install successful. However, the main npm package contains an install script that gives a helpful error.


## 1. GitHub Release

Create a [GitHub Release](https://github.com/elm/compiler/releases) with the following files:

1. `binary-for-mac-64-bit.gz`
2. `binary-for-mac-arm-64-bit.gz`
3. `binary-for-linux-64-bit.gz`
4. `binary-for-linux-arm-64-bit.gz`
5. `binary-for-windows-64-bit.gz`

Create each of these by running the `elm` executable for each platform through `gzip elm`.


## 2. Put the binaries in place

Put the above files at:

1. `packages/darwin_arm64/elm`
2. `packages/darwin_x64/elm`
3. `packages/linux_x64/elm`
4. `packages/linux_arm64/elm`
5. `packages/win32_x64/elm.exe` (Note the `.exe` file extension!)

(They are ignored by git.)


## 3. Publish the binary packages

Repeat this for all the packages mentioned in the previous section. This uses `packages/darwin_arm64` as an example.

1. Go to the folder: `cd packages/darwin_arm64`
2. Double-check that you put the right binary in the right package: `file elm`
3. Double-check that the file is executable: `ls -l elm`
4. In `package.json` of the binary package, bump the version for example to `"0.19.1-2"`.
5. In `package.json` of the main npm package, update `"optionalDependencies"` to point to the bumped version. For example: `"@elm_binaries/darwin_arm64": "0.19.1-2"`
6. Publish the package: `npm publish --access=public`

   `--access=public` is needed because scoped packages are private by default.

<details>
<summary>Notes about the versions of the binary packages</summary>

- End users never have to think about them. They only need to think about the version of the main npm package.

- The binary packages can have different versions. One can have `"0.19.1-0"` while another is at `"0.19.1-1"`. This is useful if you mess up publishing one platform: Then you can bump just that one and re-release, instead of having to re-release _all_ platforms.

- The version of the main npm package is not related to the versions of the binary packages – they’re all independent. So the main npm package can be at `"0.19.1-6"` while the binary packages have suffixes like `-0`, `-1` and `-9`. (They all share the `0.19.1` prefix though to make things more understandable!)

- The main npm package pins the versions of the binary packages _exactly_ – no version ranges.
  - This means that installing `elm@0.19.1-6` installs the exact same bytes in two years as today.
  - The `package.json` of each binary package says which OS and CPU it works for. `binary.js` in the main npm package has code that deals with OS and CPU too, so the main npm package needs to install binary packages with known OS and CPU declarations.

- There is no need to use `beta` suffixes for the binary packages. Just bump the number suffix and point to it in a beta release of the main npm package. As mentioned above:
  - Already published versions of the main npm package depend on exact versions of the binary packages, so they won’t accidentally start downloading beta versions.
  - End users only see the version of the main npm package.

</details>


## 4. Try a beta release

In `package.json`, bump the version to `"0.19.2-beta"`.

Double-check that `"optionalDependencies"` is in sync with the binary packages.

```bash
npm publish --tag beta
```

To test that it works, run these commands:

```bash
npm dist-tags ls elm
npm install elm@beta --ignore-scripts
```

The `latest` tag should not be changed, and there should be an additional `beta` tag.

Try this on Windows, Linux, and Mac.


## 5. Publish final release

Remove the `-beta` suffix from the version in `package.json`. Then run:

```bash
npm publish
```


## 6. Tag the `latest-0.19.1` version

Many compiler releases have needed multiple `npm` publications. Maybe something does not work on Windows or some dependency becomes insecure. Normal `npm` problems.

The convention for each Elm release is to create a tag the latest one.

```bash
npm dist-tag add elm@0.19.1-3 latest-0.19.1
```

That way people who want a specific version can point to `latest-0.19.1` or `latest-0.18.0` instead of knowing the particular names of all the various publications.

You can read more about dist-tags [here](https://docs.npmjs.com/cli/dist-tag).

