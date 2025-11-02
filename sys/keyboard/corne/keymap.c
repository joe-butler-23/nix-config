/*
Copyright 2019 @foostan
Copyright 2020 Drashna Jaelre <@drashna>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include QMK_KEYBOARD_H
#include "sendstring_uk.h"

// Tap Dance declarations
enum {
    TD_BRACKETS,
    TD_QUOTES,
    TD_HASH_TILD,
    TD_BACKTICK_PIPE,
    TD_SLASH_BACKSLASH,
    TD_MINUS_UNDERSCORE,
    TD_ESC_CAPS_WORD,
};

// Tap Dance functions with bracket pairs and cursor positioning
void dance_brackets(tap_dance_state_t *state, void *user_data) {
    const uint8_t mods = get_mods();
    
    clear_oneshot_mods();  // Temporarily disable mods
    unregister_mods(MOD_MASK_CSAG);
    
    switch (state->count) {
        case 1:
            SEND_STRING("()");  // Parentheses pair
            tap_code(KC_LEFT);  // Move cursor between brackets
            break;
        case 2:
            SEND_STRING("[]");  // Square brackets pair
            tap_code(KC_LEFT);  // Move cursor between brackets
            break;
        case 3:
            SEND_STRING("{}");  // Curly braces pair
            tap_code(KC_LEFT);  // Move cursor between brackets
            break;
        default:
            SEND_STRING("{}");  // Default to curly braces for 4+ taps
            tap_code(KC_LEFT);
            break;
    }
    
    register_mods(mods);  // Restore mods
}

void dance_quotes(tap_dance_state_t *state, void *user_data) {
    const uint8_t mods = get_mods();
    
    clear_oneshot_mods();  // Temporarily disable mods
    unregister_mods(MOD_MASK_CSAG);
    
    switch (state->count) {
        case 1:
            SEND_STRING("''");  // Single quotes pair
            tap_code(KC_LEFT);  // Move cursor between quotes
            break;
        case 2:
            SEND_STRING("\"\""); // Double quotes pair (using literal string)
            tap_code(KC_LEFT);    // Move cursor between quotes
            break;
        default:
            SEND_STRING("\"\""); // Default to double quotes (using literal string)
            tap_code(KC_LEFT);    // Move cursor between quotes
            break;
    }
    
    register_mods(mods);  // Restore mods
}

// New Tap Dance functions
void dance_hash_tild(tap_dance_state_t *state, void *user_data) {
    const uint8_t mods = get_mods();
    
    clear_oneshot_mods();
    unregister_mods(MOD_MASK_CSAG);
    
    switch (state->count) {
        case 1:
            SEND_STRING("#"); // # (using literal string)
            break;
        case 2:
            SEND_STRING("~"); // ~ (using literal string)
            break;
        default:
            SEND_STRING("#"); // Default to #
            break;
    }
    
    register_mods(mods);
}

void dance_backtick_pipe(tap_dance_state_t *state, void *user_data) {
    const uint8_t mods = get_mods();
    
    clear_oneshot_mods();
    unregister_mods(MOD_MASK_CSAG);
    
    switch (state->count) {
        case 1:
            SEND_STRING("`");  // ` (Backtick, using literal string)
            break;
        case 2:
            SEND_STRING("|"); // | (Pipe, using literal string)
            break;
        case 3:
            SEND_STRING("```"); // ``` (Triple backticks for Rmd files)
            break;
        default:
            SEND_STRING("```"); // Default to triple backticks for 4+ taps
            break;
    }
    
    register_mods(mods);
}

// New Tap Dance function for slash and backslash
void dance_slash_backslash(tap_dance_state_t *state, void *user_data) {
    const uint8_t mods = get_mods();
    
    clear_oneshot_mods();
    unregister_mods(MOD_MASK_CSAG);
    
    switch (state->count) {
        case 1:
            SEND_STRING("/"); // / (using literal string)
            break;
        case 2:
            SEND_STRING("\\"); // \ (Backslash, using literal string)
            break;
        default:
            SEND_STRING("/"); // Default to forward slash
            break;
    }
    
    register_mods(mods);
}

// New Tap Dance function for minus and underscore
void dance_minus_underscore(tap_dance_state_t *state, void *user_data) {
    const uint8_t mods = get_mods();
    
    clear_oneshot_mods();
    unregister_mods(MOD_MASK_CSAG);
    
    switch (state->count) {
        case 1:
            SEND_STRING("-"); // - (Minus, using literal string)
            break;
        case 2:
            SEND_STRING("_"); // _ (Underscore, using literal string)
            break;
        default:
            SEND_STRING("-"); // Default to minus
            break;
    }
    
    register_mods(mods);
}

// New Tap Dance function for ESC/Caps Word
void dance_esc_caps_word(tap_dance_state_t *state, void *user_data) {
    switch (state->count) {
        case 1:
            tap_code(KC_ESC); // Single tap = ESC
            break;
        case 2:
            caps_word_toggle(); // Double tap = toggle caps word
            break;
        default:
            tap_code(KC_ESC); // Default to ESC
            break;
    }
}

// Tap Dance definitions
tap_dance_action_t tap_dance_actions[] = {
    [TD_BRACKETS] = ACTION_TAP_DANCE_FN(dance_brackets),
    [TD_QUOTES] = ACTION_TAP_DANCE_FN(dance_quotes),
    [TD_HASH_TILD] = ACTION_TAP_DANCE_FN(dance_hash_tild),
    [TD_BACKTICK_PIPE] = ACTION_TAP_DANCE_FN(dance_backtick_pipe),
    [TD_SLASH_BACKSLASH] = ACTION_TAP_DANCE_FN(dance_slash_backslash),
    [TD_MINUS_UNDERSCORE] = ACTION_TAP_DANCE_FN(dance_minus_underscore),
    [TD_ESC_CAPS_WORD] = ACTION_TAP_DANCE_FN(dance_esc_caps_word),
};

const char chordal_hold_layout[MATRIX_ROWS][MATRIX_COLS] PROGMEM =
    LAYOUT_split_3x6_3_ex2(
    // Left hand main block (non-thumbs)
    'L', 'L', 'L', 'L', 'L', 'L', // KC_TAB to KC_T
    '*',                          // LT(3, KC_NO) - Left Thumb 1

    // Right hand main block (non-thumbs)
    '*',                          // LT(3, KC_NO) - Right Thumb 1 (mirrored)
    'R', 'R', 'R', 'R', 'R', 'R', // KC_Y to KC_BSPC (assuming KC_BSPC is on pinky)

    // Left hand main block
    'L', 'L', 'L', 'L', 'L', 'L', // KC_ESC to KC_G
    '*',                          // TG(1) - Left Thumb 2

    // Right hand main block
    '*',                          // MO(2) - Right Thumb 2 (mirrored)
    'R', 'R', 'R', 'R', 'R', 'R', // KC_H to KC_QUOT

    // Left hand main block
    'L', 'L', 'L', 'L', 'L', 'L', // KC_LSFT to KC_B

    // Right hand main block
    'R', 'R', 'R', 'R', 'R', 'R', // KC_N to KC_ENT

    // Left hand thumbs
    '*', '*', '*',                // MO(2), MO(1), KC_ENT

    // Right hand thumbs
    '*', '*', '*'                 // KC_SPC, MO(1), MO(2)
);

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

    [0] = LAYOUT_split_3x6_3_ex2(KC_TAB, KC_Q, KC_W, KC_E, KC_R, KC_T, LT(3, KC_NO), LT(3, KC_NO), KC_Y, KC_U, KC_I, KC_O, KC_P, KC_BSPC, TD(TD_ESC_CAPS_WORD), LT(4,KC_A), LGUI_T(KC_S), LCTL_T(KC_D), LSFT_T(KC_F), KC_G, TG(1), MO(2), KC_H, RSFT_T(KC_J), RCTL_T(KC_K), RGUI_T(KC_L), LALT_T(KC_SCLN), KC_QUOT, KC_LSFT, KC_Z, KC_X, KC_C, KC_V, KC_B, KC_N, KC_M, KC_COMM, KC_DOT, TD(TD_SLASH_BACKSLASH), KC_ENT, MO(2), MO(1), KC_ENT, KC_SPC, MO(1), MO(2)),

    [1] = LAYOUT_split_3x6_3_ex2(KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, TD(TD_BRACKETS), KC_1, KC_2, KC_3, KC_RBRC, KC_BSPC, KC_NO, TD(TD_BACKTICK_PIPE), TD(TD_HASH_TILD), TD(TD_QUOTES), TD(TD_BRACKETS), KC_NO, TG(1), KC_NO, KC_SCLN, KC_4, KC_5, KC_6, KC_EQL, KC_ENT, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_GRV, KC_7, KC_8, KC_9, KC_BSLS, KC_NO, KC_NO, KC_NO, KC_NO, KC_EQL, KC_0, TD(TD_MINUS_UNDERSCORE)),

    [2] = LAYOUT_split_3x6_3_ex2(KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, TD(TD_QUOTES), KC_EXLM, KC_AT, KC_HASH, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_TRNS, KC_NO, KC_TILD, KC_DLR, KC_PERC, KC_CIRC, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_PIPE, KC_AMPR, KC_ASTR, KC_PLUS, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_UNDS, KC_PLUS, KC_LCBR),

    [3] = LAYOUT_split_3x6_3_ex2(QK_BOOT, UG_NEXT, UG_TOGG, KC_MUTE, KC_VOLD, KC_VOLU, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, DT_PRNT, DT_UP, DT_DOWN, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_SPC, KC_ENT, KC_NO, KC_NO),

    [4] = LAYOUT_split_3x6_3_ex2(KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_PGUP, KC_PGDN, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_LEFT, KC_DOWN, KC_UP, KC_RIGHT, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO)
};

#ifdef ENCODER_MAP_ENABLE
const uint16_t PROGMEM encoder_map[][NUM_ENCODERS][NUM_DIRECTIONS] = {
  [0] = { ENCODER_CCW_CW(KC_VOLD, KC_VOLU), ENCODER_CCW_CW(KC_MPRV, KC_MNXT), ENCODER_CCW_CW(RM_VALD, RM_VALU), ENCODER_CCW_CW(KC_RGHT, KC_LEFT), },
  [1] = { ENCODER_CCW_CW(KC_VOLD, KC_VOLU), ENCODER_CCW_CW(KC_MPRV, KC_MNXT), ENCODER_CCW_CW(RM_VALD, RM_VALU), ENCODER_CCW_CW(KC_RGHT, KC_LEFT), },
  [2] = { ENCODER_CCW_CW(KC_VOLD, KC_VOLU), ENCODER_CCW_CW(KC_MPRV, KC_MNXT), ENCODER_CCW_CW(RM_VALD, RM_VALU), ENCODER_CCW_CW(KC_RGHT, KC_LEFT), },
  [3] = { ENCODER_CCW_CW(KC_VOLD, KC_VOLU), ENCODER_CCW_CW(KC_MPRV, KC_MNXT), ENCODER_CCW_CW(RM_VALD, RM_VALU), ENCODER_CCW_CW(KC_RGHT, KC_LEFT), },
};
#endif
