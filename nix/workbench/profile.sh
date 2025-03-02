usage_profile() {
     usage "profile" "Cluster profile operations" <<EOF
    list                  List profile names (json)
    all-profiles | all    All profile contents (json)
    compose NAME..        Create a profile composed from named profiles
    json NAME             Get contents of either named profile, or profile JSON desc
    describe NAME         Print a human description of a profile
    node-specs PROFILE-NAME/JSON
                          Print node specs JSON for the given profile and environment
    allocate-time PROFILE-NAME/JSON
                          Allocate time for a run of a profile
EOF
}

global_profile_eras=(
    shelley
    allegra
    mary
    alonzo
    babbage
)

profile() {
local op=${1:-show}; test $# -gt 0 && shift

case "$op" in
    list | names | ls )
        profile generate-all | jq 'keys'
        ;;

    all-profiles | generate-all | all )
        with_era_profiles '
          map (profiles(.; null; null; []))
          | add
        ';;

    all-profile-names | names | all-names )
        with_era_profiles '
          map (profile_names(.; null; null; []))
          | add
        ';;

    has-profile )
        local usage="USAGE: wb profile $op NAME"
        local name=${1:?$usage}

        with_era_profiles '
          map (has_profile(.; null; null; []; $name))
          | any
        ' --exit-status --arg name "$name" >/dev/null
        ;;

    ## XXX:  does not respect overlays!!
    compose )
        local profile_names="$@"

        profile generate-all |
        jq --argjson profile_names "$(to_jsonlist ${profile_names[*]})" '
          . as $profiles
          | $profile_names | debug
          | map($profiles[.])
          | add
          ';;

    json | show )
        local usage="USAGE: wb profile $op [NAME=<current-shell-profile>"
        local name=${1:-${WORKBENCH_SHELL_PROFILE:?variable unset, no profile name to use as a default.}}

        local json=$(if test -f  "$name"
                     then jq '.' "$name"
                     else profile generate-all |
                             jq '.["'$name'"]'
                     fi)

        local preset=$(jq -r '.preset // ""' <<<$json)
        local preset_overlay=$global_basedir/profiles/presets/$preset/overlay.json
        if test -z "$preset" -o ! -f $preset_overlay
        then echo "$json"
        else jq '. * $overlay[0]' <<<$json --slurpfile overlay $preset_overlay
        fi;;

    describe )
        local usage="USAGE: wb profile $op NAME"
        local name=${1:?$usage}

        profile json $name |
        jq 'include "derived";

           profile_pretty_describe(.)
           ' --raw-output -L "$global_basedir/profiles" -L "$global_basedir";;

    has-preset )
        local usage="USAGE: wb profile $op NAME"
        local profile=${1:?$usage}
        profile json "$profile" | jqtest ".preset != null";;

    preset )
        local usage="USAGE: wb profile $op NAME"
        local profile=${1:?$usage}
        profile json "$profile" | jq -r '.preset // ""';;

    preset-get-file )
        local usage="USAGE: wb profile $op PRESET-NAME DESC FILE"
        local preset=${1:?$usage}
        local   desc=${2:?$usage}
        local   file=${3:?$usage}
        local preset_dir=$global_basedir/profiles/presets/$preset
        local       path=$preset_dir/$file

        if   test ! -d "$preset_dir"; then fail "unknown preset: $preset"
        elif test ! -f "$path";       then fail "preset $preset has no file: $file"
        else echo "$path"
        fi;;

    node-specs )
        local usage="USAGE: wb profile $op PROFILE-NAME/JSON"
        local profile=${1:?$usage}

        local args=(
            -L "$global_basedir"
            --argjson env "$WORKBENCH_ENV"
        )
        ## WARNING: this is structured in correspondence
        ##          with the output generated by cardano-topology
        profile json "$profile" | jq '
          include "profile-run";
          . as $prof
          | profile_node_specs($env; $prof)
        ' "${args[@]}";;

    allocate-time )
        local usage="USAGE: wb profile $op PROFILE-JSON"
        local profile=${1:?$usage}

        if profile has-preset "$profile"
        then
            local preset=$(profile json "$profile" | jq '.preset' -r)
            local shelley=$(profile preset-get-file "$preset" 'genesis' 'genesis/genesis-shelley.json')
            local start=$(jq '.systemStart | fromdateiso8601' $shelley)
            local offset="$((start - $(date +%s))) seconds"
            local start_tag=$(date --date=@$(date +%s) --utc +'%Y'-'%m'-'%d'-'%H.%M')
        else
            local offset=$(profile json "$profile" |
                               jq '.derived.genesis_future_offset' --raw-output)
            local start=$(date '+%s' --date="now + $offset")
            local start_tag=$(date --date=@$start --utc +'%Y'-'%m'-'%d'-'%H.%M')
        fi
        local args=(
            -L "$global_basedir"
            --arg 'future_offset'   "$offset"
            --arg 'start'           "$start"
            --arg 'start_human'     "$(date --date=@$start --utc +"%Y-%m-%dT%H:%M:%SZ")"
            --arg 'start_tag'       "$start_tag"
            --arg 'systemStart'     "$(date --date=@$start --utc --iso-8601=s | cut -c-19)Z"
        )

        profile json "$profile" | jq '
          include "profile-run";

          . as $prof
          | profile_timing($prof;
                           $future_offset;
                           $start;
                           $start_human;
                           $start_tag;
                           $systemStart)
        ' "${args[@]}";;

    describe-timing )
        local usage="USAGE: wb profile $op TIMING-JSONEXPR"
        local timing=${1:?$usage}

        jq <<<$timing -L "$global_basedir" --raw-output '
          include "profile-run";

          timing_pretty_describe(.)
        ';;

    * ) usage_profile;; esac
}

with_era_profiles() {
    local usage="USAGE: wb profile with-profiles JQEXP"
    local jqexp=${1:?$usage}; shift

    jq  -L "$global_basedir/profiles" -L "$global_basedir" \
        --argjson eras "$(to_jsonlist ${global_profile_eras[*]})" \
        --null-input '
       include "profiles";

       $eras | '"$jqexp" "$@"
}
