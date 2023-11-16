#!/bin/node

import * as child_process from "child_process";

const CONFIG = { verbose: false }

function run(pkg) {
    if (CONFIG.verbose) console.log("npm view --json " + pkg);
    const out = child_process.spawnSync("npm", ["view", "--json", pkg]);
    const jso = JSON.parse(out.stdout.toString());
    return jso;
}

function readableSize(size) {
    return size < 1024 ? size : `${Math.floor(size/1024)} KiB`;
}

function makeCache(pkg) {
    const cache = {};
    function rec(pkg) {
        if (cache[pkg]) return;
        const jso = run(pkg);
        cache[pkg] = jso;
        for (let dep of Object.keys(jso.dependencies || {})) rec(dep);
    }
    rec(pkg)
    const jso1 = cache[pkg];
    const size = readableSize(Object.entries(cache).map(x => x[1].dist.unpackedSize).reduce((x, y) => x+y, 0));
    const deps = Object.keys(cache).join(" ");
    console.log("Name       :", jso1.name);
    console.log("Summary    :", jso1.description);
    console.log("Version    :", jso1.version);
    console.log("Uploaded   :", jso1.time.modified);
    console.log("URL        :", jso1.homepage);
    console.log("Total Size :", size);
    console.log("Total Deps :", deps);
}

function main() {
    const args = process.argv.slice(2);
    let pkg, help;
    for (let i=0; i<args.length; i++) {
        if (args[i] == "-v") CONFIG.verbose = true;
        if (args[i] == "-h" || args[i] == "--h") help = true;
        else pkg = args[i];
    }
    if (help || !pkg) {
        console.log("Usage: npm-pkginfo [-h] [-v] PACKAGE");
        if (!pkg) console.log("ERROR: package name not given")
        return;
    }
    makeCache(pkg);
}

main();
