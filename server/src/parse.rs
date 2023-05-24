use itertools::Itertools;
use ropey::Rope;
use tower_lsp::lsp_types::*;

enum CommentParserState {
    Outside,
    MaybeInside,
    JustEntered,
    Inside,
    MaybeOutside,
}

/// `start` is the char index where the keyword starts.
/// Example: if comment is produced for a function 'Main', the index would be the index of the
/// character 'M'
pub fn read_comment(content: &Rope, start: usize) -> Option<String> {
    read_multi_singleline_comment(content, start).or(read_multiline_comment(content, start))
}

pub fn read_multiline_comment(content: &Rope, start: usize) -> Option<String> {
    let mut current_idx = start;
    let mut current_state = CommentParserState::Outside;
    let mut start_idx = 0;
    let mut end_idx = 0;

    let iter = content.chars_at(start).reversed();
    for c in iter {
        match current_state {
            CommentParserState::Outside => match c {
                '/' => current_state = CommentParserState::MaybeInside,
                // TODO: more sophisticated way to decide when should stop looking for comment
                // problem as of now is that circom doesn't hold exact information on what line is
                // a definition declared
                '}' | ';' => return None,
                _ => (),
            },
            CommentParserState::MaybeInside => match c {
                '*' => {
                    current_state = CommentParserState::JustEntered;
                    end_idx = current_idx;
                }
                '/' => (),
                _ => current_state = CommentParserState::Outside,
            },
            CommentParserState::JustEntered => match c {
                '*' => end_idx = current_idx,
                _ => current_state = CommentParserState::Inside,
            },
            CommentParserState::Inside => {
                if let '*' = c {
                    current_state = CommentParserState::MaybeOutside;
                    start_idx = current_idx;
                }
            }
            CommentParserState::MaybeOutside => match c {
                '/' => {
                    let result = Itertools::intersperse(
                        content
                            .slice(start_idx..end_idx - 1)
                            .to_string()
                            .lines()
                            .map(|x| x.trim_matches(|c: char| c.is_whitespace() || c == '*')),
                        "\n",
                    )
                    .collect::<String>()
                    .trim()
                    .to_string();

                    return if !result.is_empty() {
                        Some(result)
                    } else {
                        None
                    };
                }
                '*' => (),
                _ => current_state = CommentParserState::Inside,
            },
        }

        current_idx -= 1;
    }

    None
}

pub fn read_multi_singleline_comment(content: &Rope, start: usize) -> Option<String> {
    let mut current_line_idx = content
        .try_char_to_line(start)
        .expect("char start index should be valid");
    let mut first_comment_line = 0;
    let mut last_comment_line = 0;
    let mut entered = false;

    while current_line_idx > 0 {
        current_line_idx -= 1;

        let line = content.line(current_line_idx);
        let is_line_comment = {
            let as_string = line.to_string();
            let trimmed = as_string.trim();

            let starts_as_comment = trimmed.len() >= 2 && &trimmed[0..2] == "//";
            let suspicious = trimmed.ends_with('}') || trimmed.ends_with(';');

            starts_as_comment || {
                if suspicious {
                    return None;
                } else {
                    false
                }
            }
        };

        match (entered, is_line_comment) {
            (false, true) => {
                entered = true;
                last_comment_line = current_line_idx;
            }
            (true, false) => {
                first_comment_line = current_line_idx;
                break;
            }
            _ => (),
        }
    }

    if entered {
        let start = content.line_to_char(first_comment_line);
        let end = content.line_to_char(last_comment_line + 1);
        let result = Itertools::intersperse(
            content
                .slice(start..end)
                .to_string()
                .lines()
                .map(|x| x.trim_matches(|c: char| c.is_whitespace() || c == '/')),
            "\n",
        )
        .collect::<String>()
        .trim()
        .to_string();

        Some(result)
    } else {
        None
    }
}

/// Returns the word and the index of the first character at `position`, if exists.
pub fn find_word(rope: &Rope, position: Position) -> ropey::Result<Option<(usize, String)>> {
    let char_idx = position_to_char(rope, position)?;
    let char = rope
        .get_char(char_idx)
        .expect("char_idx should not be out of range since position_to_char guarantees");

    if char.is_alphanumeric() {
        let start = {
            let mut i = char_idx;
            for c in rope.chars_at(char_idx).reversed() {
                if !c.is_alphanumeric() && c != '_' {
                    break;
                }
                i -= 1;
            }
            i
        };
        let end = {
            let mut i = char_idx;
            for c in rope.chars_at(char_idx) {
                if !c.is_alphanumeric() && c != '_' {
                    break;
                }
                i += 1;
            }
            i
        };

        Ok(Some((start, rope.slice(start..end).to_string())))
    } else {
        Ok(None)
    }
}

pub fn position_to_char(rope: &Rope, position: Position) -> ropey::Result<usize> {
    let line_start = rope.try_line_to_char(usize::try_from(position.line).unwrap())?;
    let char = line_start + usize::try_from(position.character).unwrap();

    // ensure resulting character is in bounds
    rope.try_char_to_byte(char)?;
    Ok(char)
}

pub fn char_to_position(rope: &Rope, idx: usize) -> ropey::Result<Position> {
    let line = rope.try_char_to_line(idx)?;
    let line_start = rope.line_to_char(line);
    let character = idx - line_start;

    let line = u32::try_from(line).unwrap();
    let character = u32::try_from(character).unwrap();

    Ok(Position { line, character })
}

pub fn char_range_to_position_range(
    rope: &Rope,
    range: std::ops::Range<usize>,
) -> ropey::Result<Range> {
    Ok(Range {
        start: char_to_position(rope, range.start)?,
        end: char_to_position(rope, range.end)?,
    })
}

/// Circom stores filenames surronded by double quotes
pub fn circom_filename_to_uri(s: &str) -> Url {
    // strip first and last chars because circom is stupid
    let fixed = {
        let mut chars = s.chars();
        chars.next();
        chars.next_back();

        chars.as_str()
    };

    Url::from_file_path(fixed).expect("string is valid uri")
}

/// Copied and modified from Circom.
/// Failure means there is an unclosed comment.
pub fn preprocess(expr: &str) -> Result<String, ()> {
    let mut pp = String::new();
    let mut state = 0;
    let mut loc = 0;
    let mut block_start = 0;

    let mut it = expr.chars();
    while let Some(c0) = it.next() {
        loc += 1;
        match (state, c0) {
            (0, '/') => {
                loc += 1;
                match it.next() {
                    Some('/') => {
                        state = 1;
                        pp.push(' ');
                        pp.push(' ');
                    }
                    Some('*') => {
                        block_start = loc;
                        state = 2;
                        pp.push(' ');
                        pp.push(' ');
                    }
                    Some(c1) => {
                        pp.push(c0);
                        pp.push(c1);
                    }
                    None => {
                        pp.push(c0);
                        break;
                    }
                }
            }
            (0, _) => pp.push(c0),
            (1, '\n') => {
                pp.push(c0);
                state = 0;
            }
            (2, '*') => {
                loc += 1;
                let mut next = it.next();
                while next == Some('*') {
                    pp.push(' ');
                    loc += 1;
                    next = it.next();
                }
                match next {
                    Some('/') => {
                        pp.push(' ');
                        pp.push(' ');
                        state = 0;
                    }
                    Some(c) => {
                        pp.push(' ');
                        for _i in 0..c.len_utf8() {
                            pp.push(' ');
                        }
                    }
                    None => {}
                }
            }
            (_, c) => {
                for _i in 0..c.len_utf8() {
                    pp.push(' ');
                }
            }
        }
    }
    if state == 2 {
        Err(())
    } else {
        Ok(pp)
    }
}

pub fn version_string(version: (usize, usize, usize)) -> String {
    format!("{}.{}.{}", version.0, version.1, version.2)
}
