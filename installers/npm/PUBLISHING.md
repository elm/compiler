# Publishing

Here's how to update the `npm` installer.


## 1. GitHub Release

Create a [GitHub Release](https://github.com/elm/compiler/releases) with the following files:

1. `binary-for-mac-64-bit.gz`
2. `binary-for-windows-64-bit.gz`
3. `binary-for-linux-64-bit.gz`

Create each of these by running the `elm` executable for each platform through `gzip elm`.


## 2. Try a beta release

In `package.json`, bump the version to `"0.19.2-beta"`.

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


## 3. Publish final release

Remove the `-beta` suffix from the version in `package.json`. Then run:

```bash
npm publish
```


## 4. Tag the `latest-0.19.1` version

Many compiler releases have needed multiple `npm` publications. Maybe something does not work on Windows or some dependency becomes insecure. Normal `npm` problems.

The convention for each Elm release is to create a tag the latest one.

```bash
npm dist-tag add elm@0.19.1-3 latest-0.19.1
```

That way people who want a specific version can point to `latest-0.19.1` or `latest-0.18.0` instead of knowing the particular names of all the various publications.

You can read more about dist-tags [here](https://docs.npmjs.com/cli/dist-tag).

