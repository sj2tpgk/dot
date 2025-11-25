$env.SHELL = (which nu | first | get path)
$env.PROMPT_COMMAND = { myprompt }
$env.PROMPT_COMMAND_RIGHT = ""
$env.PROMPT_INDICATOR = ""

def j [...args] { to json|^jq ...$args|from json }
def ll [...args] { ls -la }

let fish_like_theme = {
    # check https://raw.githubusercontent.com/nushell/nushell/refs/heads/main/crates/nu-utils/src/default_files/default_config.nu

    binary    : def
    block     : def
    bool      : w
    cell-path : u
    closure   : def
    comment   : m
    datetime  : w
    duration  : w
    filesize  : w
    float     : w
    header    : w
    hints     : m
    int       : w
    list      : def
    nothing   : def
    range     : w
    record    : def
    row_index : def
    separator : def
    string    : y

    search_result: def
    leading_trailing_space_bg: def

    shape_binary               : def
    shape_block                : def
    shape_bool                 : w
    shape_closure              : green_bold
    shape_custom               : green
    shape_datetime             : cyan_bold
    shape_directory            : cyan
    shape_external             : r # unmatched commands
    shape_external_resolved    : c
    shape_externalarg          : u
    shape_filepath             : u
    shape_flag                 : u
    shape_float                : w
    shape_garbage              : r # errors
    shape_glob_interpolation   : cyan_bold
    shape_globpattern          : u
    shape_int                  : w
    shape_internalcall         : w
    shape_keyword              : cyan_bold
    shape_list                 : cyan_bold
    shape_literal              : w
    shape_match_pattern        : green
    shape_matching_brackets    : { attr: u }
    shape_nothing              : light_cyan
    shape_operator             : w
    shape_pipe                 : purple_bold
    shape_range                : w
    shape_raw_string           : light_purple
    shape_record               : cyan_bold
    shape_redirection          : purple_bold
    shape_signature            : w
    shape_string               : y
    shape_string_interpolation : cyan_bold
    shape_table                : blue_bold
    shape_vardecl              : purple
    shape_variable             : u

}

let fish_completer = {|spans|
    fish -c $"complete '--do-complete=($spans | str replace -a "'" "\\'" | str join ' ')'"
    | from tsv --flexible --noheaders --no-infer
    | rename value description
    | update value {|row|
      let value = $row.value
      let need_quote = ['\' ',' '[' ']' '(' ')' ' ' '\t' "'" '"' "`"] | any {$in in $value}
      if ($need_quote and ($value | path exists)) {
        let expanded_path = if ($value starts-with ~) {$value | path expand --no-symlink} else {$value}
        $'"($expanded_path | str replace --all "\"" "\\\"")"'
      } else {$value}
    }
}

$env.config = {
    show_banner: false
    highlight_resolved_externals: true
    color_config: $fish_like_theme
    table: {
        mode: "none" # also: light, single, markdown
        index_mode: "auto"
    }
    error_style: "fancy" # also: plain
    completions: {
        external: {
            enable: true
            completer: $fish_completer
        }
    }
}

