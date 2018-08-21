# Publishing a new release

A new version of Elm came out. Huzzah! Here's how to update the `npm` installer.

## 1. Create tarballs of binaries

You can find a list of what binaries we'll need to tar up in `index.js`.

For example:

```javascript
var root =
  "https://github.com/elm/compiler/releases/download/" +
  binVersion +
  "/binaries-for-";

module.exports = binwrap({
  binaries: ["elm"],
  urls: {
    "darwin-x64": root + "mac.tar.gz",
    "win32-x64": root + "windows.tar.gz",
    "win32-ia32": root + "windows.tar.gz",
    "linux-x64": root + "linux.tar.gz"
  }
});
```

If this is the end of your `index.js`, you'll need to create these files:

1. `binaries-for-mac.tar.gz`
2. `binaries-for-windows.tar.gz`
3. `binaries-for-linux.tar.gz`

Each of these tarballs should have **only the Elm binary** inside them - no
directories!

So create them by making a directory, putting all the binaries in it, `cd`-ing
into that directory, and then running something like this:

```shell
$ tar cvzf binaries-for-linux.tar.gz elm
```

Make sure each tarball contains all the binaries listed in that `binaries:` list
in `index.js`. (The Windows ones should have `.exe` at the end; `binwrap`
expects that they will, for Windows only.)

## 2. Update the `bin/` binary wrappers

Inside the npm installer's `bin/` directory, there should be a file for each of
the binaries that will be included in this release.

Each of these must be executable! If you're not sure whether they are,
run `chmod +x` on them just to be sure.

Their paths must also must all be listed in `package.json` in two places:

1. The `"files":` field
2. The `"bin":` field

If the executables are the same as they were for the last release, great!
You can proceed to the next step. If any binaries were removed, make sure to
remove them from these lists!

## 3. Update `package.json` for a beta release

In `package.json`, bump the version to the next applicable release, and add
a `"-beta"` suffix to it.

For example, if it was on `"0.18.0"` you might bump it to `"0.19.0-beta"`.
The version number should match the release of Elm, such that if people do
`npm install elm@0.19.0@beta` they get what they would expect.

## 4. Tag the beta release

Commit this change and tag it with the name of the release **without** the
`-beta` suffix. (We will overwrite this tag later.)

For example:

```shell
$ git tag 0.19.0
$ git push origin 0.19.0
```

Now this tag should exist on GitHub, allowing us to upload binaries to it.

## 5. Upload binaries

Visit the [Create a New Release](https://github.com/elm-lang/elm-platform/releases/new)
page and use the `Tag version` dropdown to select the tag you just pushed. Give
it a title like `0.19.0`. Don't mention the `-beta` in it. The "beta" concept
is for `npm` only.

Upload the tarballs you created in step 1.

## 6. Publish beta release

Run this to publish the beta release. The `--tag beta` is **crucial** here.
Without it, `npm` will by default publish a new top-level release, which would
mean that what you just published would become what everyone gets when they
`npm install -g elm` without any additional qualifiers.

```shell
$ npm publish --tag beta
```

Afterwards you should be able to do `npm info elm | less` and see something
like this in the JSON:

```
'dist-tags': { latest: '0.18.0', beta: '0.19.0-beta' }
```

If you messed this up, and the `latest` tag now points to the beta you just
published, don't panic - it's fixable! `dist-tags` can always be modified after
the fact. Read up on `npm` [dist-tags](https://docs.npmjs.com/cli/dist-tag)
to learn how to fix things.

## 7. Verify beta installer

Make an empty directory and run `npm init` inside it.

Then run this:

```shell
$ npm install elm@beta --ignore-scripts
```

This should succeed with an exit code of `0`.
If it did, look in `node_modules/.bin/` for the binaries you expect.
They should be present, and they should also work as expected when you run them.
Because you installed them with `--ignore-scripts`, the first thing they should
do is to download themselves and then execute whatever command you requested
(e.g. `node_modules/.bin/elm make Main.elm`). If you run the same command a
second time, it should run faster because it doesn't have to download the binary
first.

Now try it again with `--ignore-scripts` turned off:

```shell
$ rm -r node_modules
$ npm install elm@beta --ignore-scripts=false
```

This time it should download the binaries during the installation phase. Once
again you should be able to run the binaries from `node_modules/.bin/`, and
this time they should be fast from the first run because they're already
downloaded.

## 8. Publish for real

It's a good idea to ask others to try out the beta installer before doing this!
Especially on multiple operating systems.

To publish the real version:

1. Edit `package.json` to remove the `-beta` suffix from the version.
2. Commit that change and push it.
3. Use `git tag --force` to overwrite the previous tag (e.g. `0.19.0` - whatever you used before).
4. Force push the tag, e.g. `git push origin 0.19.0 --force-with-lease`.
5. `npm publish`

You're done! Now whenever anyone does `npm install -g elm` they'll get the
version you just uploaded.

The reason we only used the `-beta` suffix for `npm` was so that when we ran
tests on the beta version, it was all against the same (non-beta) URLs we'd end
up using for the real version. This means there's no opportunity for us to
introduce some sort of mismatch between the beta that we verified and the real
version.
