{ fetchurl ? pkgs.fetchurl ,runCommand ? pkgs.runCommand , pkgs ? import <nixpkgs> {} }:
let 
  fu = n: "http://ishtar.tcita.com/~jonored/huntdicts/" + n;
  paths = [ 
  (fetchurl {
    url = fu "dictWordsAnagram.dawg";
    sha256 = "0gpbi3fb6k237mw7c2y2fkv5c6d9mwivpj9yibn686plraa78nnz";
  })
  (fetchurl {
    url = fu "dictWordsCross.dawg";
    sha256 = "02x0z3ni39wh7gfxhi4gh3bwvz7idimh0n0pp6calbw8k2riar1l";
  })
  (fetchurl {
    url = fu "onelook-justletters.anagram.mm.dawg";
    sha256 = "1v3mkm6cmjg0cwc65b0n6igwwdr0nm7f3nqcsn4bbzf0qykczalx";
  })
  (fetchurl {
    url = fu "onelook-justletters.cross.mm.dawg";
    sha256 = "1l30k6i4489wjw1fna1b8k6sgf09x6b8098sz4byv3np2plwcb5p";
  })
  (fetchurl {
    url = fu "sowpods-anagram.dawg";
    sha256 = "0zi80yf1y6bzbc3nfbbrz99mkcfdpdnpapms2shvc9bynji1sq4c";
  })
  (fetchurl {
    url = fu "sowpods-cross.dawg";
    sha256 = "1px08caqaf9jf7alm0w5a2fhxr48ccf776al6vbq1g6233hwx5g4";
  })
  (fetchurl {
    url = fu "ukacd-anagram.dawg";
    sha256 = "0a5mhdapp9k8c547fm05m8fkjgjh5ss1gf79dn475r4fwg14y8iw";
  })
  (fetchurl {
    url = fu "ukacd-cross.dawg";
    sha256 = "06sj29byn279g2qxck3ayjmj72wifv7lb6sxngijlnp43qxpnc5g";
  })
];
in
runCommand "hunttools-dicts" {preferLocalBuild = true; inherit paths;} ''
  echo $out
  echo $paths
  mkdir -p $out
  for i in $paths; do
    ln -s $i $out/$(basename $i | cut -d- -f2-)
  done
''
