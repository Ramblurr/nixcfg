{ pkgs, ... }:
let
  config = ../configs/home-ops/recyclarr-plato.yml;
in
pkgs.runCommand "recyclarr-policy"
  {
    nativeBuildInputs = [
      pkgs.jq
      pkgs.yq-go
    ];
  }
  ''
    stale_ids=(
      7878c33f1963fefb3d6c8657d46c2f0a
      1f733af03141f068a540eec352589a89
      27954b0a80aab882522a88a4d9eae1cd
      6d0d8de7b57e35518ac0308b0ddf404e
      bb019e1cd00f304f80971c965de064dc
      3e2c4e748b64a1a1118e0ea3f4cf6875
      3497799d29a085e2ac2df9d468413c94
      a3d82cbef5039f8d295478d28a887159
      2a7e3be05d3861d6df7171ec74cad727
      b974a6cd08c1066250f1f177d7aa1225
      dfb86d5941bc9075d6af23b09c2aeecd
      e61e28db95d22bedcadf030b8f156d96
      2a4d9069cc1fe3242ff9bdaebed239bb
      08d6d8834ad9ec87b1dc7ec8148e7a1f
      90cedc1fea7ea5d11298bebd3d1d3223
      e23edd2482476e595fb990b12e7c609c
      58d6a88f13e2db7f5059c41047876f00
      55d53828b9d81cbe20b02efd00aa0efd
      a3e19f8f627608af0211acd02bf89735
    )

    for id in "''${stale_ids[@]}"; do
      if grep -Fq "$id" ${config}; then
        echo "stale TRaSH ID remains: $id" >&2
        exit 1
      fi
    done

    for id in \
      505d871304820ba7106b693be6fe4a9e \
      0c4b99df9206d2cfac3c05ab897dd62a \
      493b6d1dbec3c3364c59d7607f7e3405 \
      caa37d0df9c348912df1fb1d88f9273a \
      e204b80c87be9497a8a6eaff48f72905 \
      b337d6812e06c200ec9a2d3cfa9d20a7; do
      grep -Fq "$id" ${config}
    done

    yq -o=json '
      .sonarr.smain.quality_profiles[] |
      select(.name == "SD")
    ' ${config} | jq --exit-status '
      .qualities == [
        {"name": "WEB 480p", "qualities": ["WEBDL-480p", "WEBRip-480p"]},
        {"name": "DVD"},
        {"name": "SDTV"}
      ]
    ' >/dev/null

    yq -o=json '
      .sonarr.smain.quality_profiles[] |
      select(.name == "WEB-1080p")
    ' ${config} | jq --exit-status '
      .qualities == [
        {"name": "WEB 1080p", "qualities": ["WEBDL-1080p", "WEBRip-1080p"]}
      ]
    ' >/dev/null

    touch "$out"
  ''
