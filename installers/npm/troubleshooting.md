# Troubleshooting

I very highly recommend asking for help on [the Elm slack](https://elmlang.herokuapp.com).

There are a lot of things that can go wrong when installing software, and it can really help to have a second pair of eyes on your situation!

This document goes through a couple options that may help you out.

<br/>


## Can you skip npm entirely?

The most reliable way to get Elm installed using the official installers for Mac and Windows [here][download].

You can also download the binaries directly. On Linux, you could do it in the terminal like this:

```bash
cd ~/Desktop/
curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
gunzip elm.gz                # unzip the file
chmod +x elm                 # make the file executable
sudo mv elm /usr/local/bin/  # put the executable in a directory likely to be listed in your PATH variable
```

If these exact commands do not work for you, you can try to do the same thing by hand.

Read the section below on `PATH` variables if you are not sure what that is!

[download]: https://github.com/elm/compiler/releases/tag/0.19.1

<br/>


## Do you need to use npm for some reason?

The company running npm has a list of common troubleshooting situations [here](https://npm.community/c/support/troubleshooting), but it may be better to just try to find your specific case on Stack Overflow. Often there are permissions issues where you may need to use `sudo` with some command.

### Firewalls

Some companies have a firewall.

These companies usually have set the `HTTP_PROXY` or `HTTPS_PROXY` environment variable on your computer. This is more common with Windows computers.

The result is that the request for `https://github.com/elm/compiler/releases/download/0.19.1/binary-for-windows-64-bit.gz` is being sent to a "proxy server" where they monitor traffic. Maybe they rule out certain domains, maybe they check data when it comes back from the actual URL, etc.

It is probably best to ask someone about the situation on this, but you can test things out by temporarily using an alternate `HTTPS_PROXY` value with something like this:

```
# Mac and Linux
HTTPS_PROXY=http://proxy.example.com npm install -g elm

# Windows
set HTTPS_PROXY=http://proxy.example.com
npm install -g elm
```

Check out [this document](https://www.npmjs.com/package/request#controlling-proxy-behaviour-using-environment-variables) for more information on how environment variables like `NO_PROXY`, `HTTP_PROXY`, and `HTTPS_PROXY` are handled by the npm.

<br/>


## Do you know what a `PATH` variable is?

When you run a command like `elm make src/Main.elm`, your computer starts by trying to find a file called `elm`.

The `PATH` is a list of directories to search within. On Mac and Linux, you can see these directories by running:

```
$ echo $PATH
/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin
```

The are separated by `:` for some reason. So running `elm make src/Main.elm` starts by searching the `PATH` for files named `elm`. On my computer, it finds `/usr/local/bin/elm` and then can actually run the command.

Is `elm` in one of the directories listed in your `PATH` variable? I recommend asking for help if you are in this scenario and unsure how to proceed.
