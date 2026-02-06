# source: https://github.com/CRTified/zsh-johnnydecimal/tree/master
# SPDX-Identifier: GPL-3.0-or-later
if [[ -z "$JOHNNYDECIMAL_BASE" ]];
then
    echo "No Johnny.Decimal basedir given";
    echo "Using ~/johnny";

    mkdir ~/johnny;
    export JOHNNYDECIMAL_BASE=~/johnny;
fi;

_johnny_fetchpath()
{
    unset retval;
    local targetpath=("${JOHNNYDECIMAL_BASE}/${_j_area} "*"/${_j_category} "*"/${_j_category}.${_j_unique} "*);
    retval=$(realpath ${targetpath});
}

_johnny_splitdecimal()
{
    _j_category=${1:0:2}
    _j_unique=${1:3:2}

    _j_area_lower=$(expr "(" ${_j_category} / 10 ")" "*" 10)
    _j_area_upper=$(($_j_area_lower + 9));
    _j_area="${(l:2::0:)_j_area_lower}-${(l:2::0:)_j_area_upper}";
}

jcd()
{
    if [[ ! $# -eq 1 ]];
    then
    echo "Usage:";
    echo "$ jcd CATEGORY.UNIQUE"
    return;
    fi;
    _johnny_splitdecimal "$1";
    _johnny_fetchpath;
    pushd "${retval}";
}

jcp()
{
    if [[ $# -lt 2 ]];
    then
    echo "Usage:";
    echo "$ jcp CATEGORY.UNIQUE SRC"
    return;
    fi;
    _johnny_splitdecimal "$1";
    _johnny_fetchpath;
    cp --no-clobber --verbose --recursive "${@:2}" "$retval";
}

jmv()
{
    if [[ $# -lt 2 ]];
    then
    echo "Usage:";
    echo "$ jcp CATEGORY.UNIQUE SRC"
    return;
    fi;
    _johnny_splitdecimal "$1";
    _johnny_fetchpath;
    mv --no-clobber --verbose "${@:2}" "$retval";
}


jmkarea()
{
    if [[ ! $# -eq 2 ]];
    then
    echo "Usage:";
    echo "$ jmkarea CATEGORY DESC";
    return;
    fi;

    _johnny_splitdecimal "${1}.00";
    mkdir --verbose "${JOHNNYDECIMAL_BASE}/${_j_area} ${2}"
}


jmkcat()
{
    if [[ ! $# -eq 2 ]];
    then
    echo "Usage:";
    echo "$ jmkcat CATEGORY DESC";
    return;
    fi;

    _johnny_splitdecimal "$1";
    local targetpath=("${JOHNNYDECIMAL_BASE}/${_j_area} "*);
    mkdir --verbose "$targetpath/${_j_category} ${2}";
}

jmkuni()
{
    if [[ ! $# -eq 2 ]];
    then
    echo "Usage:";
    echo "$ jmkuni CATEGORY.UNIQUE DESC";
    return;
    fi;

    _johnny_splitdecimal "$1";
    local targetpath=("${JOHNNYDECIMAL_BASE}/${_j_area} "*"/${_j_category} "*);
    mkdir --verbose "$targetpath/${_j_category}.${_j_unique} ${2}"
}


jwd()
{
    if [[ $# -lt 2 ]];
    then
    echo "Usage:";
    echo "$ jwd CATEGORY.UNIQUE CMD [ARG [ARG [ARG ...]]]"
    return;
    fi;
    _johnny_splitdecimal "$1";
    _johnny_fetchpath;
    pushd "${retval}" > /dev/null;
    echo "Directory: ${retval}";
    ${2} "${@:3}";
    local retval=$?;
    popd > /dev/null;
    return $retval;
}

_johnny_completeID()
{
    local state line;
    typeset -A opt_args;

    _arguments -C \
           '1: :->cats' \
           '*: :->args';

    case "$state" in
    (cats)
        for i in $JOHNNYDECIMAL_BASE/*/*;
        do
        local jdidlist=();
        local jddesclist=();
        local category=$(basename "$i");
        for j in "$i"/*; do
            local uniq=$(basename "$j");
            jdidlist+=("${uniq:0:5}");
            jddesclist+=("${uniq}");
        done;
        compadd -l -a -J "$category" -X "$category" -d jddesclist jdidlist;
        done;
        ;;
    *)
        _files
        ;;
    esac;
}

# fzf-based interactive navigation
jcdi()
{
    local selected
    selected=$(find "$JOHNNYDECIMAL_BASE" -mindepth 3 -maxdepth 3 -type d 2>/dev/null | \
        sort | \
        fzf \
            --height=70% \
            --layout=reverse \
            --border \
            --info=inline \
            --no-separator \
            --delimiter '/' \
            --with-nth=-3.. \
            --preview 'ls -la {}' \
            --preview-window='down,45%,wrap')

    if [[ -n "$selected" ]]; then
        pushd "$selected"
    fi
}

# fzf-based tree navigation
_jcdi_tree()
{
    local base="$1"
    local dir rel name depth slashes
    find "$base" -mindepth 1 -maxdepth 3 -type d 2>/dev/null | sort | while read -r dir; do
        rel="${dir#$base/}"
        name="${dir:t}"
        slashes="${rel//[^\/]}"
        depth="${#slashes}"

        case $depth in
            0)  printf '%s\t%s\n' "$dir" "$name" ;;
            1)  printf '%s\t  %s\n' "$dir" "$name" ;;
            2)  printf '%s\t    %s\n' "$dir" "$name" ;;
        esac
    done
}

jcdi2()
{
    local selected
    selected=$(_jcdi_tree "$JOHNNYDECIMAL_BASE" | \
        fzf \
            --height=70% \
            --layout=reverse \
            --border \
            --info=inline \
            --no-separator \
            --no-sort \
            --delimiter=$'\t' \
            --with-nth=2 \
            --preview 'eza -1 --no-quotes --group-directories-first {1}' \
            --preview-window='down,45%,wrap' | \
        cut -f1)

    if [[ -n "$selected" ]]; then
        pushd "$selected"
    fi
}

compdef _johnny_completeID jcd;
compdef _johnny_completeID jcp;
compdef _johnny_completeID jmv;
compdef _johnny_completeID jwd;
