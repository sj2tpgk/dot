#! /usr/bin/env node

// https://qiita.com/okuoku/items/eb21fac7c27a56e0a659
//
// har はブラウザが通信した内容を json 形式で保存したもの
// 動画の断片である ts を取り出す

if (process.argv.length < 3) {
    console.log("Usage: ./haropen.js <har-file> [ext1 ext2 ...]");
    process.exit(0);
}

const fs = require('fs');
const path = require('path');

const harFile = process.argv[2];
const exts = process.argv.slice(3).map(s => s.toLowerCase());
const har = JSON.parse(fs.readFileSync(harFile).toString("utf8"));
const entries = har.log.entries; // ★ ファイルの log.entries にリクエストの一覧がある

const outDir = process.argv[2].replace(/\.har$/, "");
if (!fs.existsSync(outDir)) fs.mkdirSync(outDir);

const urls = {}; // ★ URLをキーにした辞書
const files = []; // ★ 全ファイルを含む配列

entries.forEach(e => {
    let entry = false;
    const url = new URL(e.request.url);
    const name = url.pathname;
    const basename = path.basename(url.pathname);
    if(e.response.content.text){
        if(e.response.content.encoding == "base64"){
            entry = new Buffer.from(e.response.content.text, "base64");
        }else{
            entry = e.response.content.text;
        }
    }
    files.push({name: name, ext: path.extname(name),
                basename: basename,
                blob: entry});
    urls[url] = entry;
});

// Get files
const seen = {};
for (e of files) {
    // console.log("Extracting: " + e.basename);
    if (exts.length == 0 || exts.includes(e.ext.substring(1).toLowerCase())) {
        try {
            let name = e.basename.substring(0, Math.min(100, e.basename.length));
            if (seen[name]) { seen[name]++; name += "_" + seen[name]; }
            else seen[name] = 1;
            fs.writeFileSync(outDir + "/" + name, e.blob);
        } catch(e) { console.log(e) }
    }
}

// Get ".m3u8"          : obtain using developer tool.
// Get "encryption.key" : obtain using developer tool.
