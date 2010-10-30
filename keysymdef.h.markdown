---
layout: default
title:  keysymdef.h
---

<p><a href='keysymdef.h'>Original source</a></p>
<!-- Generator: GNU source-highlight 3.1.3
by Lorenzo Bettini
http://www.lorenzobettini.it
http://www.gnu.org/software/src-highlite -->
<pre><tt><i><font color="#9A1900">/***********************************************************</font></i>
<i><font color="#9A1900">Copyright 1987, 1994, 1998  The Open Group</font></i>

<i><font color="#9A1900">Permission to use, copy, modify, distribute, and sell this software and its</font></i>
<i><font color="#9A1900">documentation for any purpose is hereby granted without fee, provided that</font></i>
<i><font color="#9A1900">the above copyright notice appear in all copies and that both that</font></i>
<i><font color="#9A1900">copyright notice and this permission notice appear in supporting</font></i>
<i><font color="#9A1900">documentation.</font></i>

<i><font color="#9A1900">The above copyright notice and this permission notice shall be included</font></i>
<i><font color="#9A1900">in all copies or substantial portions of the Software.</font></i>

<i><font color="#9A1900">THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS</font></i>
<i><font color="#9A1900">OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF</font></i>
<i><font color="#9A1900">MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.</font></i>
<i><font color="#9A1900">IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR</font></i>
<i><font color="#9A1900">OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,</font></i>
<i><font color="#9A1900">ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR</font></i>
<i><font color="#9A1900">OTHER DEALINGS IN THE SOFTWARE.</font></i>

<i><font color="#9A1900">Except as contained in this notice, the name of The Open Group shall</font></i>
<i><font color="#9A1900">not be used in advertising or otherwise to promote the sale, use or</font></i>
<i><font color="#9A1900">other dealings in this Software without prior written authorization</font></i>
<i><font color="#9A1900">from The Open Group.</font></i>


<i><font color="#9A1900">Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts</font></i>

<i><font color="#9A1900">                        All Rights Reserved</font></i>

<i><font color="#9A1900">Permission to use, copy, modify, and distribute this software and its</font></i>
<i><font color="#9A1900">documentation for any purpose and without fee is hereby granted,</font></i>
<i><font color="#9A1900">provided that the above copyright notice appear in all copies and that</font></i>
<i><font color="#9A1900">both that copyright notice and this permission notice appear in</font></i>
<i><font color="#9A1900">supporting documentation, and that the name of Digital not be</font></i>
<i><font color="#9A1900">used in advertising or publicity pertaining to distribution of the</font></i>
<i><font color="#9A1900">software without specific, written prior permission.</font></i>

<i><font color="#9A1900">DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING</font></i>
<i><font color="#9A1900">ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL</font></i>
<i><font color="#9A1900">DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR</font></i>
<i><font color="#9A1900">ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,</font></i>
<i><font color="#9A1900">WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,</font></i>
<i><font color="#9A1900">ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS</font></i>
<i><font color="#9A1900">SOFTWARE.</font></i>

<i><font color="#9A1900">******************************************************************/</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * The "X11 Window System Protocol" standard defines in Appendix A the</font></i>
<i><font color="#9A1900"> * keysym codes. These 29-bit integer values identify characters or</font></i>
<i><font color="#9A1900"> * functions associated with each key (e.g., via the visible</font></i>
<i><font color="#9A1900"> * engraving) of a keyboard layout. This file assigns mnemonic macro</font></i>
<i><font color="#9A1900"> * names for these keysyms.</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> * This file is also compiled (by src/util/makekeys.c in libX11) into</font></i>
<i><font color="#9A1900"> * hash tables that can be accessed with X11 library functions such as</font></i>
<i><font color="#9A1900"> * XStringToKeysym() and XKeysymToString().</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> * Where a keysym corresponds one-to-one to an ISO 10646 / Unicode</font></i>
<i><font color="#9A1900"> * character, this is noted in a comment that provides both the U+xxxx</font></i>
<i><font color="#9A1900"> * Unicode position, as well as the official Unicode name of the</font></i>
<i><font color="#9A1900"> * character.</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> * Where the correspondence is either not one-to-one or semantically</font></i>
<i><font color="#9A1900"> * unclear, the Unicode position and name are enclosed in</font></i>
<i><font color="#9A1900"> * parentheses. Such legacy keysyms should be considered deprecated</font></i>
<i><font color="#9A1900"> * and are not recommended for use in future keyboard mappings.</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> * For any future extension of the keysyms with characters already</font></i>
<i><font color="#9A1900"> * found in ISO 10646 / Unicode, the following algorithm shall be</font></i>
<i><font color="#9A1900"> * used. The new keysym code position will simply be the character's</font></i>
<i><font color="#9A1900"> * Unicode number plus 0x01000000. The keysym values in the range</font></i>
<i><font color="#9A1900"> * 0x01000100 to 0x0110ffff are reserved to represent Unicode</font></i>
<i><font color="#9A1900"> * characters in the range U+0100 to U+10FFFF.</font></i>
<i><font color="#9A1900"> * </font></i>
<i><font color="#9A1900"> * While most newer Unicode-based X11 clients do already accept</font></i>
<i><font color="#9A1900"> * Unicode-mapped keysyms in the range 0x01000100 to 0x0110ffff, it</font></i>
<i><font color="#9A1900"> * will remain necessary for clients -- in the interest of</font></i>
<i><font color="#9A1900"> * compatibility with existing servers -- to also understand the</font></i>
<i><font color="#9A1900"> * existing legacy keysym values in the range 0x0100 to 0x20ff.</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> * Where several mnemonic names are defined for the same keysym in this</font></i>
<i><font color="#9A1900"> * file, all but the first one listed should be considered deprecated.</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> * Mnemonic names for keysyms are defined in this file with lines</font></i>
<i><font color="#9A1900"> * that match one of these Perl regular expressions:</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> *    /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*\/\* U+([0-9A-F]{4,6}) (.*) \*\/\s*$/</font></i>
<i><font color="#9A1900"> *    /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*\/\*\(U+([0-9A-F]{4,6}) (.*)\)\*\/\s*$/</font></i>
<i><font color="#9A1900"> *    /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*(\/\*\s*(.*)\s*\*\/)?\s*$/</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> * Before adding new keysyms, please do consider the following: In</font></i>
<i><font color="#9A1900"> * addition to the keysym names defined in this file, the</font></i>
<i><font color="#9A1900"> * XStringToKeysym() and XKeysymToString() functions will also handle</font></i>
<i><font color="#9A1900"> * any keysym string of the form "U0020" to "U007E" and "U00A0" to</font></i>
<i><font color="#9A1900"> * "U10FFFF" for all possible Unicode characters. In other words,</font></i>
<i><font color="#9A1900"> * every possible Unicode character has already a keysym string</font></i>
<i><font color="#9A1900"> * defined algorithmically, even if it is not listed here. Therefore,</font></i>
<i><font color="#9A1900"> * defining an additional keysym macro is only necessary where a</font></i>
<i><font color="#9A1900"> * non-hexadecimal mnemonic name is needed, or where the new keysym</font></i>
<i><font color="#9A1900"> * does not represent any existing Unicode character.</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> * When adding new keysyms to this file, do not forget to also update the</font></i>
<i><font color="#9A1900"> * following:</font></i>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> *   - the mappings in src/KeyBind.c in the repo</font></i>
<i><font color="#9A1900"> *     </font></i><u><font color="#0000FF">git://anongit.freedesktop.org/xorg/lib/libX11</font></u>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> *   - the protocol specification in specs/XProtocol/X11.keysyms</font></i>
<i><font color="#9A1900"> *     in the repo </font></i><u><font color="#0000FF">git://anongit.freedesktop.org/xorg/doc/xorg-docs</font></u>
<i><font color="#9A1900"> *</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#define</font></b> XK_VoidSymbol                  <font color="#993399">0xffffff</font>  <i><font color="#9A1900">/* Void symbol */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_MISCELLANY
<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * TTY function keys, cleverly chosen to map to ASCII, for convenience of</font></i>
<i><font color="#9A1900"> * programming, but could have been arbitrary (at the cost of lookup</font></i>
<i><font color="#9A1900"> * tables in client code).</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#define</font></b> XK_BackSpace                     <font color="#993399">0xff08</font>  <i><font color="#9A1900">/* Back space, back char */</font></i>
<b><font color="#000080">#define</font></b> XK_Tab                           <font color="#993399">0xff09</font>
<b><font color="#000080">#define</font></b> XK_Linefeed                      <font color="#993399">0xff0a</font>  <i><font color="#9A1900">/* Linefeed, LF */</font></i>
<b><font color="#000080">#define</font></b> XK_Clear                         <font color="#993399">0xff0b</font>
<b><font color="#000080">#define</font></b> XK_Return                        <font color="#993399">0xff0d</font>  <i><font color="#9A1900">/* Return, enter */</font></i>
<b><font color="#000080">#define</font></b> XK_Pause                         <font color="#993399">0xff13</font>  <i><font color="#9A1900">/* Pause, hold */</font></i>
<b><font color="#000080">#define</font></b> XK_Scroll_Lock                   <font color="#993399">0xff14</font>
<b><font color="#000080">#define</font></b> XK_Sys_Req                       <font color="#993399">0xff15</font>
<b><font color="#000080">#define</font></b> XK_Escape                        <font color="#993399">0xff1b</font>
<b><font color="#000080">#define</font></b> XK_Delete                        <font color="#993399">0xffff</font>  <i><font color="#9A1900">/* Delete, rubout */</font></i>



<i><font color="#9A1900">/* International &amp; multi-key character composition */</font></i>

<b><font color="#000080">#define</font></b> XK_Multi_key                     <font color="#993399">0xff20</font>  <i><font color="#9A1900">/* Multi-key character compose */</font></i>
<b><font color="#000080">#define</font></b> XK_Codeinput                     <font color="#993399">0xff37</font>
<b><font color="#000080">#define</font></b> XK_SingleCandidate               <font color="#993399">0xff3c</font>
<b><font color="#000080">#define</font></b> XK_MultipleCandidate             <font color="#993399">0xff3d</font>
<b><font color="#000080">#define</font></b> XK_PreviousCandidate             <font color="#993399">0xff3e</font>

<i><font color="#9A1900">/* Japanese keyboard support */</font></i>

<b><font color="#000080">#define</font></b> XK_Kanji                         <font color="#993399">0xff21</font>  <i><font color="#9A1900">/* Kanji, Kanji convert */</font></i>
<b><font color="#000080">#define</font></b> XK_Muhenkan                      <font color="#993399">0xff22</font>  <i><font color="#9A1900">/* Cancel Conversion */</font></i>
<b><font color="#000080">#define</font></b> XK_Henkan_Mode                   <font color="#993399">0xff23</font>  <i><font color="#9A1900">/* Start/Stop Conversion */</font></i>
<b><font color="#000080">#define</font></b> XK_Henkan                        <font color="#993399">0xff23</font>  <i><font color="#9A1900">/* Alias for Henkan_Mode */</font></i>
<b><font color="#000080">#define</font></b> XK_Romaji                        <font color="#993399">0xff24</font>  <i><font color="#9A1900">/* to Romaji */</font></i>
<b><font color="#000080">#define</font></b> XK_Hiragana                      <font color="#993399">0xff25</font>  <i><font color="#9A1900">/* to Hiragana */</font></i>
<b><font color="#000080">#define</font></b> XK_Katakana                      <font color="#993399">0xff26</font>  <i><font color="#9A1900">/* to Katakana */</font></i>
<b><font color="#000080">#define</font></b> XK_Hiragana_Katakana             <font color="#993399">0xff27</font>  <i><font color="#9A1900">/* Hiragana/Katakana toggle */</font></i>
<b><font color="#000080">#define</font></b> XK_Zenkaku                       <font color="#993399">0xff28</font>  <i><font color="#9A1900">/* to Zenkaku */</font></i>
<b><font color="#000080">#define</font></b> XK_Hankaku                       <font color="#993399">0xff29</font>  <i><font color="#9A1900">/* to Hankaku */</font></i>
<b><font color="#000080">#define</font></b> XK_Zenkaku_Hankaku               <font color="#993399">0xff2a</font>  <i><font color="#9A1900">/* Zenkaku/Hankaku toggle */</font></i>
<b><font color="#000080">#define</font></b> XK_Touroku                       <font color="#993399">0xff2b</font>  <i><font color="#9A1900">/* Add to Dictionary */</font></i>
<b><font color="#000080">#define</font></b> XK_Massyo                        <font color="#993399">0xff2c</font>  <i><font color="#9A1900">/* Delete from Dictionary */</font></i>
<b><font color="#000080">#define</font></b> XK_Kana_Lock                     <font color="#993399">0xff2d</font>  <i><font color="#9A1900">/* Kana Lock */</font></i>
<b><font color="#000080">#define</font></b> XK_Kana_Shift                    <font color="#993399">0xff2e</font>  <i><font color="#9A1900">/* Kana Shift */</font></i>
<b><font color="#000080">#define</font></b> XK_Eisu_Shift                    <font color="#993399">0xff2f</font>  <i><font color="#9A1900">/* Alphanumeric Shift */</font></i>
<b><font color="#000080">#define</font></b> XK_Eisu_toggle                   <font color="#993399">0xff30</font>  <i><font color="#9A1900">/* Alphanumeric toggle */</font></i>
<b><font color="#000080">#define</font></b> XK_Kanji_Bangou                  <font color="#993399">0xff37</font>  <i><font color="#9A1900">/* Codeinput */</font></i>
<b><font color="#000080">#define</font></b> XK_Zen_Koho                      <font color="#993399">0xff3d</font>  <i><font color="#9A1900">/* Multiple/All Candidate(s) */</font></i>
<b><font color="#000080">#define</font></b> XK_Mae_Koho                      <font color="#993399">0xff3e</font>  <i><font color="#9A1900">/* Previous Candidate */</font></i>

<i><font color="#9A1900">/* 0xff31 thru 0xff3f are under XK_KOREAN */</font></i>

<i><font color="#9A1900">/* Cursor control &amp; motion */</font></i>

<b><font color="#000080">#define</font></b> XK_Home                          <font color="#993399">0xff50</font>
<b><font color="#000080">#define</font></b> XK_Left                          <font color="#993399">0xff51</font>  <i><font color="#9A1900">/* Move left, left arrow */</font></i>
<b><font color="#000080">#define</font></b> XK_Up                            <font color="#993399">0xff52</font>  <i><font color="#9A1900">/* Move up, up arrow */</font></i>
<b><font color="#000080">#define</font></b> XK_Right                         <font color="#993399">0xff53</font>  <i><font color="#9A1900">/* Move right, right arrow */</font></i>
<b><font color="#000080">#define</font></b> XK_Down                          <font color="#993399">0xff54</font>  <i><font color="#9A1900">/* Move down, down arrow */</font></i>
<b><font color="#000080">#define</font></b> XK_Prior                         <font color="#993399">0xff55</font>  <i><font color="#9A1900">/* Prior, previous */</font></i>
<b><font color="#000080">#define</font></b> XK_Page_Up                       <font color="#993399">0xff55</font>
<b><font color="#000080">#define</font></b> XK_Next                          <font color="#993399">0xff56</font>  <i><font color="#9A1900">/* Next */</font></i>
<b><font color="#000080">#define</font></b> XK_Page_Down                     <font color="#993399">0xff56</font>
<b><font color="#000080">#define</font></b> XK_End                           <font color="#993399">0xff57</font>  <i><font color="#9A1900">/* EOL */</font></i>
<b><font color="#000080">#define</font></b> XK_Begin                         <font color="#993399">0xff58</font>  <i><font color="#9A1900">/* BOL */</font></i>


<i><font color="#9A1900">/* Misc functions */</font></i>

<b><font color="#000080">#define</font></b> XK_Select                        <font color="#993399">0xff60</font>  <i><font color="#9A1900">/* Select, mark */</font></i>
<b><font color="#000080">#define</font></b> XK_Print                         <font color="#993399">0xff61</font>
<b><font color="#000080">#define</font></b> XK_Execute                       <font color="#993399">0xff62</font>  <i><font color="#9A1900">/* Execute, run, do */</font></i>
<b><font color="#000080">#define</font></b> XK_Insert                        <font color="#993399">0xff63</font>  <i><font color="#9A1900">/* Insert, insert here */</font></i>
<b><font color="#000080">#define</font></b> XK_Undo                          <font color="#993399">0xff65</font>
<b><font color="#000080">#define</font></b> XK_Redo                          <font color="#993399">0xff66</font>  <i><font color="#9A1900">/* Redo, again */</font></i>
<b><font color="#000080">#define</font></b> XK_Menu                          <font color="#993399">0xff67</font>
<b><font color="#000080">#define</font></b> XK_Find                          <font color="#993399">0xff68</font>  <i><font color="#9A1900">/* Find, search */</font></i>
<b><font color="#000080">#define</font></b> XK_Cancel                        <font color="#993399">0xff69</font>  <i><font color="#9A1900">/* Cancel, stop, abort, exit */</font></i>
<b><font color="#000080">#define</font></b> XK_Help                          <font color="#993399">0xff6a</font>  <i><font color="#9A1900">/* Help */</font></i>
<b><font color="#000080">#define</font></b> XK_Break                         <font color="#993399">0xff6b</font>
<b><font color="#000080">#define</font></b> XK_Mode_switch                   <font color="#993399">0xff7e</font>  <i><font color="#9A1900">/* Character set switch */</font></i>
<b><font color="#000080">#define</font></b> XK_script_switch                 <font color="#993399">0xff7e</font>  <i><font color="#9A1900">/* Alias for mode_switch */</font></i>
<b><font color="#000080">#define</font></b> XK_Num_Lock                      <font color="#993399">0xff7f</font>

<i><font color="#9A1900">/* Keypad functions, keypad numbers cleverly chosen to map to ASCII */</font></i>

<b><font color="#000080">#define</font></b> XK_KP_Space                      <font color="#993399">0xff80</font>  <i><font color="#9A1900">/* Space */</font></i>
<b><font color="#000080">#define</font></b> XK_KP_Tab                        <font color="#993399">0xff89</font>
<b><font color="#000080">#define</font></b> XK_KP_Enter                      <font color="#993399">0xff8d</font>  <i><font color="#9A1900">/* Enter */</font></i>
<b><font color="#000080">#define</font></b> XK_KP_F1                         <font color="#993399">0xff91</font>  <i><font color="#9A1900">/* PF1, KP_A, ... */</font></i>
<b><font color="#000080">#define</font></b> XK_KP_F2                         <font color="#993399">0xff92</font>
<b><font color="#000080">#define</font></b> XK_KP_F3                         <font color="#993399">0xff93</font>
<b><font color="#000080">#define</font></b> XK_KP_F4                         <font color="#993399">0xff94</font>
<b><font color="#000080">#define</font></b> XK_KP_Home                       <font color="#993399">0xff95</font>
<b><font color="#000080">#define</font></b> XK_KP_Left                       <font color="#993399">0xff96</font>
<b><font color="#000080">#define</font></b> XK_KP_Up                         <font color="#993399">0xff97</font>
<b><font color="#000080">#define</font></b> XK_KP_Right                      <font color="#993399">0xff98</font>
<b><font color="#000080">#define</font></b> XK_KP_Down                       <font color="#993399">0xff99</font>
<b><font color="#000080">#define</font></b> XK_KP_Prior                      <font color="#993399">0xff9a</font>
<b><font color="#000080">#define</font></b> XK_KP_Page_Up                    <font color="#993399">0xff9a</font>
<b><font color="#000080">#define</font></b> XK_KP_Next                       <font color="#993399">0xff9b</font>
<b><font color="#000080">#define</font></b> XK_KP_Page_Down                  <font color="#993399">0xff9b</font>
<b><font color="#000080">#define</font></b> XK_KP_End                        <font color="#993399">0xff9c</font>
<b><font color="#000080">#define</font></b> XK_KP_Begin                      <font color="#993399">0xff9d</font>
<b><font color="#000080">#define</font></b> XK_KP_Insert                     <font color="#993399">0xff9e</font>
<b><font color="#000080">#define</font></b> XK_KP_Delete                     <font color="#993399">0xff9f</font>
<b><font color="#000080">#define</font></b> XK_KP_Equal                      <font color="#993399">0xffbd</font>  <i><font color="#9A1900">/* Equals */</font></i>
<b><font color="#000080">#define</font></b> XK_KP_Multiply                   <font color="#993399">0xffaa</font>
<b><font color="#000080">#define</font></b> XK_KP_Add                        <font color="#993399">0xffab</font>
<b><font color="#000080">#define</font></b> XK_KP_Separator                  <font color="#993399">0xffac</font>  <i><font color="#9A1900">/* Separator, often comma */</font></i>
<b><font color="#000080">#define</font></b> XK_KP_Subtract                   <font color="#993399">0xffad</font>
<b><font color="#000080">#define</font></b> XK_KP_Decimal                    <font color="#993399">0xffae</font>
<b><font color="#000080">#define</font></b> XK_KP_Divide                     <font color="#993399">0xffaf</font>

<b><font color="#000080">#define</font></b> XK_KP_0                          <font color="#993399">0xffb0</font>
<b><font color="#000080">#define</font></b> XK_KP_1                          <font color="#993399">0xffb1</font>
<b><font color="#000080">#define</font></b> XK_KP_2                          <font color="#993399">0xffb2</font>
<b><font color="#000080">#define</font></b> XK_KP_3                          <font color="#993399">0xffb3</font>
<b><font color="#000080">#define</font></b> XK_KP_4                          <font color="#993399">0xffb4</font>
<b><font color="#000080">#define</font></b> XK_KP_5                          <font color="#993399">0xffb5</font>
<b><font color="#000080">#define</font></b> XK_KP_6                          <font color="#993399">0xffb6</font>
<b><font color="#000080">#define</font></b> XK_KP_7                          <font color="#993399">0xffb7</font>
<b><font color="#000080">#define</font></b> XK_KP_8                          <font color="#993399">0xffb8</font>
<b><font color="#000080">#define</font></b> XK_KP_9                          <font color="#993399">0xffb9</font>



<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Auxiliary functions; note the duplicate definitions for left and right</font></i>
<i><font color="#9A1900"> * function keys;  Sun keyboards and a few other manufacturers have such</font></i>
<i><font color="#9A1900"> * function key groups on the left and/or right sides of the keyboard.</font></i>
<i><font color="#9A1900"> * We've not found a keyboard with more than 35 function keys total.</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#define</font></b> XK_F1                            <font color="#993399">0xffbe</font>
<b><font color="#000080">#define</font></b> XK_F2                            <font color="#993399">0xffbf</font>
<b><font color="#000080">#define</font></b> XK_F3                            <font color="#993399">0xffc0</font>
<b><font color="#000080">#define</font></b> XK_F4                            <font color="#993399">0xffc1</font>
<b><font color="#000080">#define</font></b> XK_F5                            <font color="#993399">0xffc2</font>
<b><font color="#000080">#define</font></b> XK_F6                            <font color="#993399">0xffc3</font>
<b><font color="#000080">#define</font></b> XK_F7                            <font color="#993399">0xffc4</font>
<b><font color="#000080">#define</font></b> XK_F8                            <font color="#993399">0xffc5</font>
<b><font color="#000080">#define</font></b> XK_F9                            <font color="#993399">0xffc6</font>
<b><font color="#000080">#define</font></b> XK_F10                           <font color="#993399">0xffc7</font>
<b><font color="#000080">#define</font></b> XK_F11                           <font color="#993399">0xffc8</font>
<b><font color="#000080">#define</font></b> XK_L1                            <font color="#993399">0xffc8</font>
<b><font color="#000080">#define</font></b> XK_F12                           <font color="#993399">0xffc9</font>
<b><font color="#000080">#define</font></b> XK_L2                            <font color="#993399">0xffc9</font>
<b><font color="#000080">#define</font></b> XK_F13                           <font color="#993399">0xffca</font>
<b><font color="#000080">#define</font></b> XK_L3                            <font color="#993399">0xffca</font>
<b><font color="#000080">#define</font></b> XK_F14                           <font color="#993399">0xffcb</font>
<b><font color="#000080">#define</font></b> XK_L4                            <font color="#993399">0xffcb</font>
<b><font color="#000080">#define</font></b> XK_F15                           <font color="#993399">0xffcc</font>
<b><font color="#000080">#define</font></b> XK_L5                            <font color="#993399">0xffcc</font>
<b><font color="#000080">#define</font></b> XK_F16                           <font color="#993399">0xffcd</font>
<b><font color="#000080">#define</font></b> XK_L6                            <font color="#993399">0xffcd</font>
<b><font color="#000080">#define</font></b> XK_F17                           <font color="#993399">0xffce</font>
<b><font color="#000080">#define</font></b> XK_L7                            <font color="#993399">0xffce</font>
<b><font color="#000080">#define</font></b> XK_F18                           <font color="#993399">0xffcf</font>
<b><font color="#000080">#define</font></b> XK_L8                            <font color="#993399">0xffcf</font>
<b><font color="#000080">#define</font></b> XK_F19                           <font color="#993399">0xffd0</font>
<b><font color="#000080">#define</font></b> XK_L9                            <font color="#993399">0xffd0</font>
<b><font color="#000080">#define</font></b> XK_F20                           <font color="#993399">0xffd1</font>
<b><font color="#000080">#define</font></b> XK_L10                           <font color="#993399">0xffd1</font>
<b><font color="#000080">#define</font></b> XK_F21                           <font color="#993399">0xffd2</font>
<b><font color="#000080">#define</font></b> XK_R1                            <font color="#993399">0xffd2</font>
<b><font color="#000080">#define</font></b> XK_F22                           <font color="#993399">0xffd3</font>
<b><font color="#000080">#define</font></b> XK_R2                            <font color="#993399">0xffd3</font>
<b><font color="#000080">#define</font></b> XK_F23                           <font color="#993399">0xffd4</font>
<b><font color="#000080">#define</font></b> XK_R3                            <font color="#993399">0xffd4</font>
<b><font color="#000080">#define</font></b> XK_F24                           <font color="#993399">0xffd5</font>
<b><font color="#000080">#define</font></b> XK_R4                            <font color="#993399">0xffd5</font>
<b><font color="#000080">#define</font></b> XK_F25                           <font color="#993399">0xffd6</font>
<b><font color="#000080">#define</font></b> XK_R5                            <font color="#993399">0xffd6</font>
<b><font color="#000080">#define</font></b> XK_F26                           <font color="#993399">0xffd7</font>
<b><font color="#000080">#define</font></b> XK_R6                            <font color="#993399">0xffd7</font>
<b><font color="#000080">#define</font></b> XK_F27                           <font color="#993399">0xffd8</font>
<b><font color="#000080">#define</font></b> XK_R7                            <font color="#993399">0xffd8</font>
<b><font color="#000080">#define</font></b> XK_F28                           <font color="#993399">0xffd9</font>
<b><font color="#000080">#define</font></b> XK_R8                            <font color="#993399">0xffd9</font>
<b><font color="#000080">#define</font></b> XK_F29                           <font color="#993399">0xffda</font>
<b><font color="#000080">#define</font></b> XK_R9                            <font color="#993399">0xffda</font>
<b><font color="#000080">#define</font></b> XK_F30                           <font color="#993399">0xffdb</font>
<b><font color="#000080">#define</font></b> XK_R10                           <font color="#993399">0xffdb</font>
<b><font color="#000080">#define</font></b> XK_F31                           <font color="#993399">0xffdc</font>
<b><font color="#000080">#define</font></b> XK_R11                           <font color="#993399">0xffdc</font>
<b><font color="#000080">#define</font></b> XK_F32                           <font color="#993399">0xffdd</font>
<b><font color="#000080">#define</font></b> XK_R12                           <font color="#993399">0xffdd</font>
<b><font color="#000080">#define</font></b> XK_F33                           <font color="#993399">0xffde</font>
<b><font color="#000080">#define</font></b> XK_R13                           <font color="#993399">0xffde</font>
<b><font color="#000080">#define</font></b> XK_F34                           <font color="#993399">0xffdf</font>
<b><font color="#000080">#define</font></b> XK_R14                           <font color="#993399">0xffdf</font>
<b><font color="#000080">#define</font></b> XK_F35                           <font color="#993399">0xffe0</font>
<b><font color="#000080">#define</font></b> XK_R15                           <font color="#993399">0xffe0</font>

<i><font color="#9A1900">/* Modifiers */</font></i>

<b><font color="#000080">#define</font></b> XK_Shift_L                       <font color="#993399">0xffe1</font>  <i><font color="#9A1900">/* Left shift */</font></i>
<b><font color="#000080">#define</font></b> XK_Shift_R                       <font color="#993399">0xffe2</font>  <i><font color="#9A1900">/* Right shift */</font></i>
<b><font color="#000080">#define</font></b> XK_Control_L                     <font color="#993399">0xffe3</font>  <i><font color="#9A1900">/* Left control */</font></i>
<b><font color="#000080">#define</font></b> XK_Control_R                     <font color="#993399">0xffe4</font>  <i><font color="#9A1900">/* Right control */</font></i>
<b><font color="#000080">#define</font></b> XK_Caps_Lock                     <font color="#993399">0xffe5</font>  <i><font color="#9A1900">/* Caps lock */</font></i>
<b><font color="#000080">#define</font></b> XK_Shift_Lock                    <font color="#993399">0xffe6</font>  <i><font color="#9A1900">/* Shift lock */</font></i>

<b><font color="#000080">#define</font></b> XK_Meta_L                        <font color="#993399">0xffe7</font>  <i><font color="#9A1900">/* Left meta */</font></i>
<b><font color="#000080">#define</font></b> XK_Meta_R                        <font color="#993399">0xffe8</font>  <i><font color="#9A1900">/* Right meta */</font></i>
<b><font color="#000080">#define</font></b> XK_Alt_L                         <font color="#993399">0xffe9</font>  <i><font color="#9A1900">/* Left alt */</font></i>
<b><font color="#000080">#define</font></b> XK_Alt_R                         <font color="#993399">0xffea</font>  <i><font color="#9A1900">/* Right alt */</font></i>
<b><font color="#000080">#define</font></b> XK_Super_L                       <font color="#993399">0xffeb</font>  <i><font color="#9A1900">/* Left super */</font></i>
<b><font color="#000080">#define</font></b> XK_Super_R                       <font color="#993399">0xffec</font>  <i><font color="#9A1900">/* Right super */</font></i>
<b><font color="#000080">#define</font></b> XK_Hyper_L                       <font color="#993399">0xffed</font>  <i><font color="#9A1900">/* Left hyper */</font></i>
<b><font color="#000080">#define</font></b> XK_Hyper_R                       <font color="#993399">0xffee</font>  <i><font color="#9A1900">/* Right hyper */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_MISCELLANY */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Keyboard (XKB) Extension function and modifier keys</font></i>
<i><font color="#9A1900"> * (from Appendix C of "The X Keyboard Extension: Protocol Specification")</font></i>
<i><font color="#9A1900"> * Byte 3 = 0xfe</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_XKB_KEYS
<b><font color="#000080">#define</font></b> XK_ISO_Lock                      <font color="#993399">0xfe01</font>
<b><font color="#000080">#define</font></b> XK_ISO_Level2_Latch              <font color="#993399">0xfe02</font>
<b><font color="#000080">#define</font></b> XK_ISO_Level3_Shift              <font color="#993399">0xfe03</font>
<b><font color="#000080">#define</font></b> XK_ISO_Level3_Latch              <font color="#993399">0xfe04</font>
<b><font color="#000080">#define</font></b> XK_ISO_Level3_Lock               <font color="#993399">0xfe05</font>
<b><font color="#000080">#define</font></b> XK_ISO_Level5_Shift              <font color="#993399">0xfe11</font>
<b><font color="#000080">#define</font></b> XK_ISO_Level5_Latch              <font color="#993399">0xfe12</font>
<b><font color="#000080">#define</font></b> XK_ISO_Level5_Lock               <font color="#993399">0xfe13</font>
<b><font color="#000080">#define</font></b> XK_ISO_Group_Shift               <font color="#993399">0xff7e</font>  <i><font color="#9A1900">/* Alias for mode_switch */</font></i>
<b><font color="#000080">#define</font></b> XK_ISO_Group_Latch               <font color="#993399">0xfe06</font>
<b><font color="#000080">#define</font></b> XK_ISO_Group_Lock                <font color="#993399">0xfe07</font>
<b><font color="#000080">#define</font></b> XK_ISO_Next_Group                <font color="#993399">0xfe08</font>
<b><font color="#000080">#define</font></b> XK_ISO_Next_Group_Lock           <font color="#993399">0xfe09</font>
<b><font color="#000080">#define</font></b> XK_ISO_Prev_Group                <font color="#993399">0xfe0a</font>
<b><font color="#000080">#define</font></b> XK_ISO_Prev_Group_Lock           <font color="#993399">0xfe0b</font>
<b><font color="#000080">#define</font></b> XK_ISO_First_Group               <font color="#993399">0xfe0c</font>
<b><font color="#000080">#define</font></b> XK_ISO_First_Group_Lock          <font color="#993399">0xfe0d</font>
<b><font color="#000080">#define</font></b> XK_ISO_Last_Group                <font color="#993399">0xfe0e</font>
<b><font color="#000080">#define</font></b> XK_ISO_Last_Group_Lock           <font color="#993399">0xfe0f</font>

<b><font color="#000080">#define</font></b> XK_ISO_Left_Tab                  <font color="#993399">0xfe20</font>
<b><font color="#000080">#define</font></b> XK_ISO_Move_Line_Up              <font color="#993399">0xfe21</font>
<b><font color="#000080">#define</font></b> XK_ISO_Move_Line_Down            <font color="#993399">0xfe22</font>
<b><font color="#000080">#define</font></b> XK_ISO_Partial_Line_Up           <font color="#993399">0xfe23</font>
<b><font color="#000080">#define</font></b> XK_ISO_Partial_Line_Down         <font color="#993399">0xfe24</font>
<b><font color="#000080">#define</font></b> XK_ISO_Partial_Space_Left        <font color="#993399">0xfe25</font>
<b><font color="#000080">#define</font></b> XK_ISO_Partial_Space_Right       <font color="#993399">0xfe26</font>
<b><font color="#000080">#define</font></b> XK_ISO_Set_Margin_Left           <font color="#993399">0xfe27</font>
<b><font color="#000080">#define</font></b> XK_ISO_Set_Margin_Right          <font color="#993399">0xfe28</font>
<b><font color="#000080">#define</font></b> XK_ISO_Release_Margin_Left       <font color="#993399">0xfe29</font>
<b><font color="#000080">#define</font></b> XK_ISO_Release_Margin_Right      <font color="#993399">0xfe2a</font>
<b><font color="#000080">#define</font></b> XK_ISO_Release_Both_Margins      <font color="#993399">0xfe2b</font>
<b><font color="#000080">#define</font></b> XK_ISO_Fast_Cursor_Left          <font color="#993399">0xfe2c</font>
<b><font color="#000080">#define</font></b> XK_ISO_Fast_Cursor_Right         <font color="#993399">0xfe2d</font>
<b><font color="#000080">#define</font></b> XK_ISO_Fast_Cursor_Up            <font color="#993399">0xfe2e</font>
<b><font color="#000080">#define</font></b> XK_ISO_Fast_Cursor_Down          <font color="#993399">0xfe2f</font>
<b><font color="#000080">#define</font></b> XK_ISO_Continuous_Underline      <font color="#993399">0xfe30</font>
<b><font color="#000080">#define</font></b> XK_ISO_Discontinuous_Underline   <font color="#993399">0xfe31</font>
<b><font color="#000080">#define</font></b> XK_ISO_Emphasize                 <font color="#993399">0xfe32</font>
<b><font color="#000080">#define</font></b> XK_ISO_Center_Object             <font color="#993399">0xfe33</font>
<b><font color="#000080">#define</font></b> XK_ISO_Enter                     <font color="#993399">0xfe34</font>

<b><font color="#000080">#define</font></b> XK_dead_grave                    <font color="#993399">0xfe50</font>
<b><font color="#000080">#define</font></b> XK_dead_acute                    <font color="#993399">0xfe51</font>
<b><font color="#000080">#define</font></b> XK_dead_circumflex               <font color="#993399">0xfe52</font>
<b><font color="#000080">#define</font></b> XK_dead_tilde                    <font color="#993399">0xfe53</font>
<b><font color="#000080">#define</font></b> XK_dead_perispomeni              <font color="#993399">0xfe53</font>  <i><font color="#9A1900">/* alias for dead_tilde */</font></i>
<b><font color="#000080">#define</font></b> XK_dead_macron                   <font color="#993399">0xfe54</font>
<b><font color="#000080">#define</font></b> XK_dead_breve                    <font color="#993399">0xfe55</font>
<b><font color="#000080">#define</font></b> XK_dead_abovedot                 <font color="#993399">0xfe56</font>
<b><font color="#000080">#define</font></b> XK_dead_diaeresis                <font color="#993399">0xfe57</font>
<b><font color="#000080">#define</font></b> XK_dead_abovering                <font color="#993399">0xfe58</font>
<b><font color="#000080">#define</font></b> XK_dead_doubleacute              <font color="#993399">0xfe59</font>
<b><font color="#000080">#define</font></b> XK_dead_caron                    <font color="#993399">0xfe5a</font>
<b><font color="#000080">#define</font></b> XK_dead_cedilla                  <font color="#993399">0xfe5b</font>
<b><font color="#000080">#define</font></b> XK_dead_ogonek                   <font color="#993399">0xfe5c</font>
<b><font color="#000080">#define</font></b> XK_dead_iota                     <font color="#993399">0xfe5d</font>
<b><font color="#000080">#define</font></b> XK_dead_voiced_sound             <font color="#993399">0xfe5e</font>
<b><font color="#000080">#define</font></b> XK_dead_semivoiced_sound         <font color="#993399">0xfe5f</font>
<b><font color="#000080">#define</font></b> XK_dead_belowdot                 <font color="#993399">0xfe60</font>
<b><font color="#000080">#define</font></b> XK_dead_hook                     <font color="#993399">0xfe61</font>
<b><font color="#000080">#define</font></b> XK_dead_horn                     <font color="#993399">0xfe62</font>
<b><font color="#000080">#define</font></b> XK_dead_stroke                   <font color="#993399">0xfe63</font>
<b><font color="#000080">#define</font></b> XK_dead_abovecomma               <font color="#993399">0xfe64</font>
<b><font color="#000080">#define</font></b> XK_dead_psili                    <font color="#993399">0xfe64</font>  <i><font color="#9A1900">/* alias for dead_abovecomma */</font></i>
<b><font color="#000080">#define</font></b> XK_dead_abovereversedcomma       <font color="#993399">0xfe65</font>
<b><font color="#000080">#define</font></b> XK_dead_dasia                    <font color="#993399">0xfe65</font>  <i><font color="#9A1900">/* alias for dead_abovereversedcomma */</font></i>
<b><font color="#000080">#define</font></b> XK_dead_doublegrave              <font color="#993399">0xfe66</font>
<b><font color="#000080">#define</font></b> XK_dead_belowring                <font color="#993399">0xfe67</font>
<b><font color="#000080">#define</font></b> XK_dead_belowmacron              <font color="#993399">0xfe68</font>
<b><font color="#000080">#define</font></b> XK_dead_belowcircumflex          <font color="#993399">0xfe69</font>
<b><font color="#000080">#define</font></b> XK_dead_belowtilde               <font color="#993399">0xfe6a</font>
<b><font color="#000080">#define</font></b> XK_dead_belowbreve               <font color="#993399">0xfe6b</font>
<b><font color="#000080">#define</font></b> XK_dead_belowdiaeresis           <font color="#993399">0xfe6c</font>
<b><font color="#000080">#define</font></b> XK_dead_invertedbreve            <font color="#993399">0xfe6d</font>
<b><font color="#000080">#define</font></b> XK_dead_belowcomma               <font color="#993399">0xfe6e</font>
<b><font color="#000080">#define</font></b> XK_dead_currency                 <font color="#993399">0xfe6f</font>

<i><font color="#9A1900">/* dead vowels for universal syllable entry */</font></i>
<b><font color="#000080">#define</font></b> XK_dead_a                        <font color="#993399">0xfe80</font>
<b><font color="#000080">#define</font></b> XK_dead_A                        <font color="#993399">0xfe81</font>
<b><font color="#000080">#define</font></b> XK_dead_e                        <font color="#993399">0xfe82</font>
<b><font color="#000080">#define</font></b> XK_dead_E                        <font color="#993399">0xfe83</font>
<b><font color="#000080">#define</font></b> XK_dead_i                        <font color="#993399">0xfe84</font>
<b><font color="#000080">#define</font></b> XK_dead_I                        <font color="#993399">0xfe85</font>
<b><font color="#000080">#define</font></b> XK_dead_o                        <font color="#993399">0xfe86</font>
<b><font color="#000080">#define</font></b> XK_dead_O                        <font color="#993399">0xfe87</font>
<b><font color="#000080">#define</font></b> XK_dead_u                        <font color="#993399">0xfe88</font>
<b><font color="#000080">#define</font></b> XK_dead_U                        <font color="#993399">0xfe89</font>
<b><font color="#000080">#define</font></b> XK_dead_small_schwa              <font color="#993399">0xfe8a</font>
<b><font color="#000080">#define</font></b> XK_dead_capital_schwa            <font color="#993399">0xfe8b</font>

<b><font color="#000080">#define</font></b> XK_First_Virtual_Screen          <font color="#993399">0xfed0</font>
<b><font color="#000080">#define</font></b> XK_Prev_Virtual_Screen           <font color="#993399">0xfed1</font>
<b><font color="#000080">#define</font></b> XK_Next_Virtual_Screen           <font color="#993399">0xfed2</font>
<b><font color="#000080">#define</font></b> XK_Last_Virtual_Screen           <font color="#993399">0xfed4</font>
<b><font color="#000080">#define</font></b> XK_Terminate_Server              <font color="#993399">0xfed5</font>

<b><font color="#000080">#define</font></b> XK_AccessX_Enable                <font color="#993399">0xfe70</font>
<b><font color="#000080">#define</font></b> XK_AccessX_Feedback_Enable       <font color="#993399">0xfe71</font>
<b><font color="#000080">#define</font></b> XK_RepeatKeys_Enable             <font color="#993399">0xfe72</font>
<b><font color="#000080">#define</font></b> XK_SlowKeys_Enable               <font color="#993399">0xfe73</font>
<b><font color="#000080">#define</font></b> XK_BounceKeys_Enable             <font color="#993399">0xfe74</font>
<b><font color="#000080">#define</font></b> XK_StickyKeys_Enable             <font color="#993399">0xfe75</font>
<b><font color="#000080">#define</font></b> XK_MouseKeys_Enable              <font color="#993399">0xfe76</font>
<b><font color="#000080">#define</font></b> XK_MouseKeys_Accel_Enable        <font color="#993399">0xfe77</font>
<b><font color="#000080">#define</font></b> XK_Overlay1_Enable               <font color="#993399">0xfe78</font>
<b><font color="#000080">#define</font></b> XK_Overlay2_Enable               <font color="#993399">0xfe79</font>
<b><font color="#000080">#define</font></b> XK_AudibleBell_Enable            <font color="#993399">0xfe7a</font>

<b><font color="#000080">#define</font></b> XK_Pointer_Left                  <font color="#993399">0xfee0</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Right                 <font color="#993399">0xfee1</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Up                    <font color="#993399">0xfee2</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Down                  <font color="#993399">0xfee3</font>
<b><font color="#000080">#define</font></b> XK_Pointer_UpLeft                <font color="#993399">0xfee4</font>
<b><font color="#000080">#define</font></b> XK_Pointer_UpRight               <font color="#993399">0xfee5</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DownLeft              <font color="#993399">0xfee6</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DownRight             <font color="#993399">0xfee7</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Button_Dflt           <font color="#993399">0xfee8</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Button1               <font color="#993399">0xfee9</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Button2               <font color="#993399">0xfeea</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Button3               <font color="#993399">0xfeeb</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Button4               <font color="#993399">0xfeec</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Button5               <font color="#993399">0xfeed</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DblClick_Dflt         <font color="#993399">0xfeee</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DblClick1             <font color="#993399">0xfeef</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DblClick2             <font color="#993399">0xfef0</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DblClick3             <font color="#993399">0xfef1</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DblClick4             <font color="#993399">0xfef2</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DblClick5             <font color="#993399">0xfef3</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Drag_Dflt             <font color="#993399">0xfef4</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Drag1                 <font color="#993399">0xfef5</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Drag2                 <font color="#993399">0xfef6</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Drag3                 <font color="#993399">0xfef7</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Drag4                 <font color="#993399">0xfef8</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Drag5                 <font color="#993399">0xfefd</font>

<b><font color="#000080">#define</font></b> XK_Pointer_EnableKeys            <font color="#993399">0xfef9</font>
<b><font color="#000080">#define</font></b> XK_Pointer_Accelerate            <font color="#993399">0xfefa</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DfltBtnNext           <font color="#993399">0xfefb</font>
<b><font color="#000080">#define</font></b> XK_Pointer_DfltBtnPrev           <font color="#993399">0xfefc</font>

<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_XKB_KEYS */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * 3270 Terminal Keys</font></i>
<i><font color="#9A1900"> * Byte 3 = 0xfd</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_3270
<b><font color="#000080">#define</font></b> XK_3270_Duplicate                <font color="#993399">0xfd01</font>
<b><font color="#000080">#define</font></b> XK_3270_FieldMark                <font color="#993399">0xfd02</font>
<b><font color="#000080">#define</font></b> XK_3270_Right2                   <font color="#993399">0xfd03</font>
<b><font color="#000080">#define</font></b> XK_3270_Left2                    <font color="#993399">0xfd04</font>
<b><font color="#000080">#define</font></b> XK_3270_BackTab                  <font color="#993399">0xfd05</font>
<b><font color="#000080">#define</font></b> XK_3270_EraseEOF                 <font color="#993399">0xfd06</font>
<b><font color="#000080">#define</font></b> XK_3270_EraseInput               <font color="#993399">0xfd07</font>
<b><font color="#000080">#define</font></b> XK_3270_Reset                    <font color="#993399">0xfd08</font>
<b><font color="#000080">#define</font></b> XK_3270_Quit                     <font color="#993399">0xfd09</font>
<b><font color="#000080">#define</font></b> XK_3270_PA1                      <font color="#993399">0xfd0a</font>
<b><font color="#000080">#define</font></b> XK_3270_PA2                      <font color="#993399">0xfd0b</font>
<b><font color="#000080">#define</font></b> XK_3270_PA3                      <font color="#993399">0xfd0c</font>
<b><font color="#000080">#define</font></b> XK_3270_Test                     <font color="#993399">0xfd0d</font>
<b><font color="#000080">#define</font></b> XK_3270_Attn                     <font color="#993399">0xfd0e</font>
<b><font color="#000080">#define</font></b> XK_3270_CursorBlink              <font color="#993399">0xfd0f</font>
<b><font color="#000080">#define</font></b> XK_3270_AltCursor                <font color="#993399">0xfd10</font>
<b><font color="#000080">#define</font></b> XK_3270_KeyClick                 <font color="#993399">0xfd11</font>
<b><font color="#000080">#define</font></b> XK_3270_Jump                     <font color="#993399">0xfd12</font>
<b><font color="#000080">#define</font></b> XK_3270_Ident                    <font color="#993399">0xfd13</font>
<b><font color="#000080">#define</font></b> XK_3270_Rule                     <font color="#993399">0xfd14</font>
<b><font color="#000080">#define</font></b> XK_3270_Copy                     <font color="#993399">0xfd15</font>
<b><font color="#000080">#define</font></b> XK_3270_Play                     <font color="#993399">0xfd16</font>
<b><font color="#000080">#define</font></b> XK_3270_Setup                    <font color="#993399">0xfd17</font>
<b><font color="#000080">#define</font></b> XK_3270_Record                   <font color="#993399">0xfd18</font>
<b><font color="#000080">#define</font></b> XK_3270_ChangeScreen             <font color="#993399">0xfd19</font>
<b><font color="#000080">#define</font></b> XK_3270_DeleteWord               <font color="#993399">0xfd1a</font>
<b><font color="#000080">#define</font></b> XK_3270_ExSelect                 <font color="#993399">0xfd1b</font>
<b><font color="#000080">#define</font></b> XK_3270_CursorSelect             <font color="#993399">0xfd1c</font>
<b><font color="#000080">#define</font></b> XK_3270_PrintScreen              <font color="#993399">0xfd1d</font>
<b><font color="#000080">#define</font></b> XK_3270_Enter                    <font color="#993399">0xfd1e</font>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_3270 */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Latin 1</font></i>
<i><font color="#9A1900"> * (ISO/IEC 8859-1 = Unicode U+0020..U+00FF)</font></i>
<i><font color="#9A1900"> * Byte 3 = 0</font></i>
<i><font color="#9A1900"> */</font></i>
<b><font color="#000080">#ifdef</font></b> XK_LATIN1
<b><font color="#000080">#define</font></b> XK_space                         <font color="#993399">0x0020</font>  <i><font color="#9A1900">/* U+0020 SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_exclam                        <font color="#993399">0x0021</font>  <i><font color="#9A1900">/* U+0021 EXCLAMATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_quotedbl                      <font color="#993399">0x0022</font>  <i><font color="#9A1900">/* U+0022 QUOTATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_numbersign                    <font color="#993399">0x0023</font>  <i><font color="#9A1900">/* U+0023 NUMBER SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_dollar                        <font color="#993399">0x0024</font>  <i><font color="#9A1900">/* U+0024 DOLLAR SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_percent                       <font color="#993399">0x0025</font>  <i><font color="#9A1900">/* U+0025 PERCENT SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_ampersand                     <font color="#993399">0x0026</font>  <i><font color="#9A1900">/* U+0026 AMPERSAND */</font></i>
<b><font color="#000080">#define</font></b> XK_apostrophe                    <font color="#993399">0x0027</font>  <i><font color="#9A1900">/* U+0027 APOSTROPHE */</font></i>
<b><font color="#000080">#define</font></b> XK_quoteright                    <font color="#993399">0x0027</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_parenleft                     <font color="#993399">0x0028</font>  <i><font color="#9A1900">/* U+0028 LEFT PARENTHESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_parenright                    <font color="#993399">0x0029</font>  <i><font color="#9A1900">/* U+0029 RIGHT PARENTHESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_asterisk                      <font color="#993399">0x002a</font>  <i><font color="#9A1900">/* U+002A ASTERISK */</font></i>
<b><font color="#000080">#define</font></b> XK_plus                          <font color="#993399">0x002b</font>  <i><font color="#9A1900">/* U+002B PLUS SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_comma                         <font color="#993399">0x002c</font>  <i><font color="#9A1900">/* U+002C COMMA */</font></i>
<b><font color="#000080">#define</font></b> XK_minus                         <font color="#993399">0x002d</font>  <i><font color="#9A1900">/* U+002D HYPHEN-MINUS */</font></i>
<b><font color="#000080">#define</font></b> XK_period                        <font color="#993399">0x002e</font>  <i><font color="#9A1900">/* U+002E FULL STOP */</font></i>
<b><font color="#000080">#define</font></b> XK_slash                         <font color="#993399">0x002f</font>  <i><font color="#9A1900">/* U+002F SOLIDUS */</font></i>
<b><font color="#000080">#define</font></b> XK_0                             <font color="#993399">0x0030</font>  <i><font color="#9A1900">/* U+0030 DIGIT ZERO */</font></i>
<b><font color="#000080">#define</font></b> XK_1                             <font color="#993399">0x0031</font>  <i><font color="#9A1900">/* U+0031 DIGIT ONE */</font></i>
<b><font color="#000080">#define</font></b> XK_2                             <font color="#993399">0x0032</font>  <i><font color="#9A1900">/* U+0032 DIGIT TWO */</font></i>
<b><font color="#000080">#define</font></b> XK_3                             <font color="#993399">0x0033</font>  <i><font color="#9A1900">/* U+0033 DIGIT THREE */</font></i>
<b><font color="#000080">#define</font></b> XK_4                             <font color="#993399">0x0034</font>  <i><font color="#9A1900">/* U+0034 DIGIT FOUR */</font></i>
<b><font color="#000080">#define</font></b> XK_5                             <font color="#993399">0x0035</font>  <i><font color="#9A1900">/* U+0035 DIGIT FIVE */</font></i>
<b><font color="#000080">#define</font></b> XK_6                             <font color="#993399">0x0036</font>  <i><font color="#9A1900">/* U+0036 DIGIT SIX */</font></i>
<b><font color="#000080">#define</font></b> XK_7                             <font color="#993399">0x0037</font>  <i><font color="#9A1900">/* U+0037 DIGIT SEVEN */</font></i>
<b><font color="#000080">#define</font></b> XK_8                             <font color="#993399">0x0038</font>  <i><font color="#9A1900">/* U+0038 DIGIT EIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_9                             <font color="#993399">0x0039</font>  <i><font color="#9A1900">/* U+0039 DIGIT NINE */</font></i>
<b><font color="#000080">#define</font></b> XK_colon                         <font color="#993399">0x003a</font>  <i><font color="#9A1900">/* U+003A COLON */</font></i>
<b><font color="#000080">#define</font></b> XK_semicolon                     <font color="#993399">0x003b</font>  <i><font color="#9A1900">/* U+003B SEMICOLON */</font></i>
<b><font color="#000080">#define</font></b> XK_less                          <font color="#993399">0x003c</font>  <i><font color="#9A1900">/* U+003C LESS-THAN SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_equal                         <font color="#993399">0x003d</font>  <i><font color="#9A1900">/* U+003D EQUALS SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_greater                       <font color="#993399">0x003e</font>  <i><font color="#9A1900">/* U+003E GREATER-THAN SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_question                      <font color="#993399">0x003f</font>  <i><font color="#9A1900">/* U+003F QUESTION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_at                            <font color="#993399">0x0040</font>  <i><font color="#9A1900">/* U+0040 COMMERCIAL AT */</font></i>
<b><font color="#000080">#define</font></b> XK_A                             <font color="#993399">0x0041</font>  <i><font color="#9A1900">/* U+0041 LATIN CAPITAL LETTER A */</font></i>
<b><font color="#000080">#define</font></b> XK_B                             <font color="#993399">0x0042</font>  <i><font color="#9A1900">/* U+0042 LATIN CAPITAL LETTER B */</font></i>
<b><font color="#000080">#define</font></b> XK_C                             <font color="#993399">0x0043</font>  <i><font color="#9A1900">/* U+0043 LATIN CAPITAL LETTER C */</font></i>
<b><font color="#000080">#define</font></b> XK_D                             <font color="#993399">0x0044</font>  <i><font color="#9A1900">/* U+0044 LATIN CAPITAL LETTER D */</font></i>
<b><font color="#000080">#define</font></b> XK_E                             <font color="#993399">0x0045</font>  <i><font color="#9A1900">/* U+0045 LATIN CAPITAL LETTER E */</font></i>
<b><font color="#000080">#define</font></b> XK_F                             <font color="#993399">0x0046</font>  <i><font color="#9A1900">/* U+0046 LATIN CAPITAL LETTER F */</font></i>
<b><font color="#000080">#define</font></b> XK_G                             <font color="#993399">0x0047</font>  <i><font color="#9A1900">/* U+0047 LATIN CAPITAL LETTER G */</font></i>
<b><font color="#000080">#define</font></b> XK_H                             <font color="#993399">0x0048</font>  <i><font color="#9A1900">/* U+0048 LATIN CAPITAL LETTER H */</font></i>
<b><font color="#000080">#define</font></b> XK_I                             <font color="#993399">0x0049</font>  <i><font color="#9A1900">/* U+0049 LATIN CAPITAL LETTER I */</font></i>
<b><font color="#000080">#define</font></b> XK_J                             <font color="#993399">0x004a</font>  <i><font color="#9A1900">/* U+004A LATIN CAPITAL LETTER J */</font></i>
<b><font color="#000080">#define</font></b> XK_K                             <font color="#993399">0x004b</font>  <i><font color="#9A1900">/* U+004B LATIN CAPITAL LETTER K */</font></i>
<b><font color="#000080">#define</font></b> XK_L                             <font color="#993399">0x004c</font>  <i><font color="#9A1900">/* U+004C LATIN CAPITAL LETTER L */</font></i>
<b><font color="#000080">#define</font></b> XK_M                             <font color="#993399">0x004d</font>  <i><font color="#9A1900">/* U+004D LATIN CAPITAL LETTER M */</font></i>
<b><font color="#000080">#define</font></b> XK_N                             <font color="#993399">0x004e</font>  <i><font color="#9A1900">/* U+004E LATIN CAPITAL LETTER N */</font></i>
<b><font color="#000080">#define</font></b> XK_O                             <font color="#993399">0x004f</font>  <i><font color="#9A1900">/* U+004F LATIN CAPITAL LETTER O */</font></i>
<b><font color="#000080">#define</font></b> XK_P                             <font color="#993399">0x0050</font>  <i><font color="#9A1900">/* U+0050 LATIN CAPITAL LETTER P */</font></i>
<b><font color="#000080">#define</font></b> XK_Q                             <font color="#993399">0x0051</font>  <i><font color="#9A1900">/* U+0051 LATIN CAPITAL LETTER Q */</font></i>
<b><font color="#000080">#define</font></b> XK_R                             <font color="#993399">0x0052</font>  <i><font color="#9A1900">/* U+0052 LATIN CAPITAL LETTER R */</font></i>
<b><font color="#000080">#define</font></b> XK_S                             <font color="#993399">0x0053</font>  <i><font color="#9A1900">/* U+0053 LATIN CAPITAL LETTER S */</font></i>
<b><font color="#000080">#define</font></b> XK_T                             <font color="#993399">0x0054</font>  <i><font color="#9A1900">/* U+0054 LATIN CAPITAL LETTER T */</font></i>
<b><font color="#000080">#define</font></b> XK_U                             <font color="#993399">0x0055</font>  <i><font color="#9A1900">/* U+0055 LATIN CAPITAL LETTER U */</font></i>
<b><font color="#000080">#define</font></b> XK_V                             <font color="#993399">0x0056</font>  <i><font color="#9A1900">/* U+0056 LATIN CAPITAL LETTER V */</font></i>
<b><font color="#000080">#define</font></b> XK_W                             <font color="#993399">0x0057</font>  <i><font color="#9A1900">/* U+0057 LATIN CAPITAL LETTER W */</font></i>
<b><font color="#000080">#define</font></b> XK_X                             <font color="#993399">0x0058</font>  <i><font color="#9A1900">/* U+0058 LATIN CAPITAL LETTER X */</font></i>
<b><font color="#000080">#define</font></b> XK_Y                             <font color="#993399">0x0059</font>  <i><font color="#9A1900">/* U+0059 LATIN CAPITAL LETTER Y */</font></i>
<b><font color="#000080">#define</font></b> XK_Z                             <font color="#993399">0x005a</font>  <i><font color="#9A1900">/* U+005A LATIN CAPITAL LETTER Z */</font></i>
<b><font color="#000080">#define</font></b> XK_bracketleft                   <font color="#993399">0x005b</font>  <i><font color="#9A1900">/* U+005B LEFT SQUARE BRACKET */</font></i>
<b><font color="#000080">#define</font></b> XK_backslash                     <font color="#993399">0x005c</font>  <i><font color="#9A1900">/* U+005C REVERSE SOLIDUS */</font></i>
<b><font color="#000080">#define</font></b> XK_bracketright                  <font color="#993399">0x005d</font>  <i><font color="#9A1900">/* U+005D RIGHT SQUARE BRACKET */</font></i>
<b><font color="#000080">#define</font></b> XK_asciicircum                   <font color="#993399">0x005e</font>  <i><font color="#9A1900">/* U+005E CIRCUMFLEX ACCENT */</font></i>
<b><font color="#000080">#define</font></b> XK_underscore                    <font color="#993399">0x005f</font>  <i><font color="#9A1900">/* U+005F LOW LINE */</font></i>
<b><font color="#000080">#define</font></b> XK_grave                         <font color="#993399">0x0060</font>  <i><font color="#9A1900">/* U+0060 GRAVE ACCENT */</font></i>
<b><font color="#000080">#define</font></b> XK_quoteleft                     <font color="#993399">0x0060</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_a                             <font color="#993399">0x0061</font>  <i><font color="#9A1900">/* U+0061 LATIN SMALL LETTER A */</font></i>
<b><font color="#000080">#define</font></b> XK_b                             <font color="#993399">0x0062</font>  <i><font color="#9A1900">/* U+0062 LATIN SMALL LETTER B */</font></i>
<b><font color="#000080">#define</font></b> XK_c                             <font color="#993399">0x0063</font>  <i><font color="#9A1900">/* U+0063 LATIN SMALL LETTER C */</font></i>
<b><font color="#000080">#define</font></b> XK_d                             <font color="#993399">0x0064</font>  <i><font color="#9A1900">/* U+0064 LATIN SMALL LETTER D */</font></i>
<b><font color="#000080">#define</font></b> XK_e                             <font color="#993399">0x0065</font>  <i><font color="#9A1900">/* U+0065 LATIN SMALL LETTER E */</font></i>
<b><font color="#000080">#define</font></b> XK_f                             <font color="#993399">0x0066</font>  <i><font color="#9A1900">/* U+0066 LATIN SMALL LETTER F */</font></i>
<b><font color="#000080">#define</font></b> XK_g                             <font color="#993399">0x0067</font>  <i><font color="#9A1900">/* U+0067 LATIN SMALL LETTER G */</font></i>
<b><font color="#000080">#define</font></b> XK_h                             <font color="#993399">0x0068</font>  <i><font color="#9A1900">/* U+0068 LATIN SMALL LETTER H */</font></i>
<b><font color="#000080">#define</font></b> XK_i                             <font color="#993399">0x0069</font>  <i><font color="#9A1900">/* U+0069 LATIN SMALL LETTER I */</font></i>
<b><font color="#000080">#define</font></b> XK_j                             <font color="#993399">0x006a</font>  <i><font color="#9A1900">/* U+006A LATIN SMALL LETTER J */</font></i>
<b><font color="#000080">#define</font></b> XK_k                             <font color="#993399">0x006b</font>  <i><font color="#9A1900">/* U+006B LATIN SMALL LETTER K */</font></i>
<b><font color="#000080">#define</font></b> XK_l                             <font color="#993399">0x006c</font>  <i><font color="#9A1900">/* U+006C LATIN SMALL LETTER L */</font></i>
<b><font color="#000080">#define</font></b> XK_m                             <font color="#993399">0x006d</font>  <i><font color="#9A1900">/* U+006D LATIN SMALL LETTER M */</font></i>
<b><font color="#000080">#define</font></b> XK_n                             <font color="#993399">0x006e</font>  <i><font color="#9A1900">/* U+006E LATIN SMALL LETTER N */</font></i>
<b><font color="#000080">#define</font></b> XK_o                             <font color="#993399">0x006f</font>  <i><font color="#9A1900">/* U+006F LATIN SMALL LETTER O */</font></i>
<b><font color="#000080">#define</font></b> XK_p                             <font color="#993399">0x0070</font>  <i><font color="#9A1900">/* U+0070 LATIN SMALL LETTER P */</font></i>
<b><font color="#000080">#define</font></b> XK_q                             <font color="#993399">0x0071</font>  <i><font color="#9A1900">/* U+0071 LATIN SMALL LETTER Q */</font></i>
<b><font color="#000080">#define</font></b> XK_r                             <font color="#993399">0x0072</font>  <i><font color="#9A1900">/* U+0072 LATIN SMALL LETTER R */</font></i>
<b><font color="#000080">#define</font></b> XK_s                             <font color="#993399">0x0073</font>  <i><font color="#9A1900">/* U+0073 LATIN SMALL LETTER S */</font></i>
<b><font color="#000080">#define</font></b> XK_t                             <font color="#993399">0x0074</font>  <i><font color="#9A1900">/* U+0074 LATIN SMALL LETTER T */</font></i>
<b><font color="#000080">#define</font></b> XK_u                             <font color="#993399">0x0075</font>  <i><font color="#9A1900">/* U+0075 LATIN SMALL LETTER U */</font></i>
<b><font color="#000080">#define</font></b> XK_v                             <font color="#993399">0x0076</font>  <i><font color="#9A1900">/* U+0076 LATIN SMALL LETTER V */</font></i>
<b><font color="#000080">#define</font></b> XK_w                             <font color="#993399">0x0077</font>  <i><font color="#9A1900">/* U+0077 LATIN SMALL LETTER W */</font></i>
<b><font color="#000080">#define</font></b> XK_x                             <font color="#993399">0x0078</font>  <i><font color="#9A1900">/* U+0078 LATIN SMALL LETTER X */</font></i>
<b><font color="#000080">#define</font></b> XK_y                             <font color="#993399">0x0079</font>  <i><font color="#9A1900">/* U+0079 LATIN SMALL LETTER Y */</font></i>
<b><font color="#000080">#define</font></b> XK_z                             <font color="#993399">0x007a</font>  <i><font color="#9A1900">/* U+007A LATIN SMALL LETTER Z */</font></i>
<b><font color="#000080">#define</font></b> XK_braceleft                     <font color="#993399">0x007b</font>  <i><font color="#9A1900">/* U+007B LEFT CURLY BRACKET */</font></i>
<b><font color="#000080">#define</font></b> XK_bar                           <font color="#993399">0x007c</font>  <i><font color="#9A1900">/* U+007C VERTICAL LINE */</font></i>
<b><font color="#000080">#define</font></b> XK_braceright                    <font color="#993399">0x007d</font>  <i><font color="#9A1900">/* U+007D RIGHT CURLY BRACKET */</font></i>
<b><font color="#000080">#define</font></b> XK_asciitilde                    <font color="#993399">0x007e</font>  <i><font color="#9A1900">/* U+007E TILDE */</font></i>

<b><font color="#000080">#define</font></b> XK_nobreakspace                  <font color="#993399">0x00a0</font>  <i><font color="#9A1900">/* U+00A0 NO-BREAK SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_exclamdown                    <font color="#993399">0x00a1</font>  <i><font color="#9A1900">/* U+00A1 INVERTED EXCLAMATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_cent                          <font color="#993399">0x00a2</font>  <i><font color="#9A1900">/* U+00A2 CENT SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_sterling                      <font color="#993399">0x00a3</font>  <i><font color="#9A1900">/* U+00A3 POUND SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_currency                      <font color="#993399">0x00a4</font>  <i><font color="#9A1900">/* U+00A4 CURRENCY SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_yen                           <font color="#993399">0x00a5</font>  <i><font color="#9A1900">/* U+00A5 YEN SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_brokenbar                     <font color="#993399">0x00a6</font>  <i><font color="#9A1900">/* U+00A6 BROKEN BAR */</font></i>
<b><font color="#000080">#define</font></b> XK_section                       <font color="#993399">0x00a7</font>  <i><font color="#9A1900">/* U+00A7 SECTION SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_diaeresis                     <font color="#993399">0x00a8</font>  <i><font color="#9A1900">/* U+00A8 DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_copyright                     <font color="#993399">0x00a9</font>  <i><font color="#9A1900">/* U+00A9 COPYRIGHT SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_ordfeminine                   <font color="#993399">0x00aa</font>  <i><font color="#9A1900">/* U+00AA FEMININE ORDINAL INDICATOR */</font></i>
<b><font color="#000080">#define</font></b> XK_guillemotleft                 <font color="#993399">0x00ab</font>  <i><font color="#9A1900">/* U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_notsign                       <font color="#993399">0x00ac</font>  <i><font color="#9A1900">/* U+00AC NOT SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_hyphen                        <font color="#993399">0x00ad</font>  <i><font color="#9A1900">/* U+00AD SOFT HYPHEN */</font></i>
<b><font color="#000080">#define</font></b> XK_registered                    <font color="#993399">0x00ae</font>  <i><font color="#9A1900">/* U+00AE REGISTERED SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_macron                        <font color="#993399">0x00af</font>  <i><font color="#9A1900">/* U+00AF MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_degree                        <font color="#993399">0x00b0</font>  <i><font color="#9A1900">/* U+00B0 DEGREE SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_plusminus                     <font color="#993399">0x00b1</font>  <i><font color="#9A1900">/* U+00B1 PLUS-MINUS SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_twosuperior                   <font color="#993399">0x00b2</font>  <i><font color="#9A1900">/* U+00B2 SUPERSCRIPT TWO */</font></i>
<b><font color="#000080">#define</font></b> XK_threesuperior                 <font color="#993399">0x00b3</font>  <i><font color="#9A1900">/* U+00B3 SUPERSCRIPT THREE */</font></i>
<b><font color="#000080">#define</font></b> XK_acute                         <font color="#993399">0x00b4</font>  <i><font color="#9A1900">/* U+00B4 ACUTE ACCENT */</font></i>
<b><font color="#000080">#define</font></b> XK_mu                            <font color="#993399">0x00b5</font>  <i><font color="#9A1900">/* U+00B5 MICRO SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_paragraph                     <font color="#993399">0x00b6</font>  <i><font color="#9A1900">/* U+00B6 PILCROW SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_periodcentered                <font color="#993399">0x00b7</font>  <i><font color="#9A1900">/* U+00B7 MIDDLE DOT */</font></i>
<b><font color="#000080">#define</font></b> XK_cedilla                       <font color="#993399">0x00b8</font>  <i><font color="#9A1900">/* U+00B8 CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_onesuperior                   <font color="#993399">0x00b9</font>  <i><font color="#9A1900">/* U+00B9 SUPERSCRIPT ONE */</font></i>
<b><font color="#000080">#define</font></b> XK_masculine                     <font color="#993399">0x00ba</font>  <i><font color="#9A1900">/* U+00BA MASCULINE ORDINAL INDICATOR */</font></i>
<b><font color="#000080">#define</font></b> XK_guillemotright                <font color="#993399">0x00bb</font>  <i><font color="#9A1900">/* U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_onequarter                    <font color="#993399">0x00bc</font>  <i><font color="#9A1900">/* U+00BC VULGAR FRACTION ONE QUARTER */</font></i>
<b><font color="#000080">#define</font></b> XK_onehalf                       <font color="#993399">0x00bd</font>  <i><font color="#9A1900">/* U+00BD VULGAR FRACTION ONE HALF */</font></i>
<b><font color="#000080">#define</font></b> XK_threequarters                 <font color="#993399">0x00be</font>  <i><font color="#9A1900">/* U+00BE VULGAR FRACTION THREE QUARTERS */</font></i>
<b><font color="#000080">#define</font></b> XK_questiondown                  <font color="#993399">0x00bf</font>  <i><font color="#9A1900">/* U+00BF INVERTED QUESTION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_Agrave                        <font color="#993399">0x00c0</font>  <i><font color="#9A1900">/* U+00C0 LATIN CAPITAL LETTER A WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Aacute                        <font color="#993399">0x00c1</font>  <i><font color="#9A1900">/* U+00C1 LATIN CAPITAL LETTER A WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Acircumflex                   <font color="#993399">0x00c2</font>  <i><font color="#9A1900">/* U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Atilde                        <font color="#993399">0x00c3</font>  <i><font color="#9A1900">/* U+00C3 LATIN CAPITAL LETTER A WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Adiaeresis                    <font color="#993399">0x00c4</font>  <i><font color="#9A1900">/* U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_Aring                         <font color="#993399">0x00c5</font>  <i><font color="#9A1900">/* U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_AE                            <font color="#993399">0x00c6</font>  <i><font color="#9A1900">/* U+00C6 LATIN CAPITAL LETTER AE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ccedilla                      <font color="#993399">0x00c7</font>  <i><font color="#9A1900">/* U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_Egrave                        <font color="#993399">0x00c8</font>  <i><font color="#9A1900">/* U+00C8 LATIN CAPITAL LETTER E WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Eacute                        <font color="#993399">0x00c9</font>  <i><font color="#9A1900">/* U+00C9 LATIN CAPITAL LETTER E WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ecircumflex                   <font color="#993399">0x00ca</font>  <i><font color="#9A1900">/* U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Ediaeresis                    <font color="#993399">0x00cb</font>  <i><font color="#9A1900">/* U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_Igrave                        <font color="#993399">0x00cc</font>  <i><font color="#9A1900">/* U+00CC LATIN CAPITAL LETTER I WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Iacute                        <font color="#993399">0x00cd</font>  <i><font color="#9A1900">/* U+00CD LATIN CAPITAL LETTER I WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Icircumflex                   <font color="#993399">0x00ce</font>  <i><font color="#9A1900">/* U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Idiaeresis                    <font color="#993399">0x00cf</font>  <i><font color="#9A1900">/* U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_ETH                           <font color="#993399">0x00d0</font>  <i><font color="#9A1900">/* U+00D0 LATIN CAPITAL LETTER ETH */</font></i>
<b><font color="#000080">#define</font></b> XK_Eth                           <font color="#993399">0x00d0</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Ntilde                        <font color="#993399">0x00d1</font>  <i><font color="#9A1900">/* U+00D1 LATIN CAPITAL LETTER N WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ograve                        <font color="#993399">0x00d2</font>  <i><font color="#9A1900">/* U+00D2 LATIN CAPITAL LETTER O WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Oacute                        <font color="#993399">0x00d3</font>  <i><font color="#9A1900">/* U+00D3 LATIN CAPITAL LETTER O WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ocircumflex                   <font color="#993399">0x00d4</font>  <i><font color="#9A1900">/* U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Otilde                        <font color="#993399">0x00d5</font>  <i><font color="#9A1900">/* U+00D5 LATIN CAPITAL LETTER O WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Odiaeresis                    <font color="#993399">0x00d6</font>  <i><font color="#9A1900">/* U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_multiply                      <font color="#993399">0x00d7</font>  <i><font color="#9A1900">/* U+00D7 MULTIPLICATION SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_Oslash                        <font color="#993399">0x00d8</font>  <i><font color="#9A1900">/* U+00D8 LATIN CAPITAL LETTER O WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ooblique                      <font color="#993399">0x00d8</font>  <i><font color="#9A1900">/* U+00D8 LATIN CAPITAL LETTER O WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ugrave                        <font color="#993399">0x00d9</font>  <i><font color="#9A1900">/* U+00D9 LATIN CAPITAL LETTER U WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Uacute                        <font color="#993399">0x00da</font>  <i><font color="#9A1900">/* U+00DA LATIN CAPITAL LETTER U WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ucircumflex                   <font color="#993399">0x00db</font>  <i><font color="#9A1900">/* U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Udiaeresis                    <font color="#993399">0x00dc</font>  <i><font color="#9A1900">/* U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_Yacute                        <font color="#993399">0x00dd</font>  <i><font color="#9A1900">/* U+00DD LATIN CAPITAL LETTER Y WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_THORN                         <font color="#993399">0x00de</font>  <i><font color="#9A1900">/* U+00DE LATIN CAPITAL LETTER THORN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thorn                         <font color="#993399">0x00de</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_ssharp                        <font color="#993399">0x00df</font>  <i><font color="#9A1900">/* U+00DF LATIN SMALL LETTER SHARP S */</font></i>
<b><font color="#000080">#define</font></b> XK_agrave                        <font color="#993399">0x00e0</font>  <i><font color="#9A1900">/* U+00E0 LATIN SMALL LETTER A WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_aacute                        <font color="#993399">0x00e1</font>  <i><font color="#9A1900">/* U+00E1 LATIN SMALL LETTER A WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_acircumflex                   <font color="#993399">0x00e2</font>  <i><font color="#9A1900">/* U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_atilde                        <font color="#993399">0x00e3</font>  <i><font color="#9A1900">/* U+00E3 LATIN SMALL LETTER A WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_adiaeresis                    <font color="#993399">0x00e4</font>  <i><font color="#9A1900">/* U+00E4 LATIN SMALL LETTER A WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_aring                         <font color="#993399">0x00e5</font>  <i><font color="#9A1900">/* U+00E5 LATIN SMALL LETTER A WITH RING ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ae                            <font color="#993399">0x00e6</font>  <i><font color="#9A1900">/* U+00E6 LATIN SMALL LETTER AE */</font></i>
<b><font color="#000080">#define</font></b> XK_ccedilla                      <font color="#993399">0x00e7</font>  <i><font color="#9A1900">/* U+00E7 LATIN SMALL LETTER C WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_egrave                        <font color="#993399">0x00e8</font>  <i><font color="#9A1900">/* U+00E8 LATIN SMALL LETTER E WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_eacute                        <font color="#993399">0x00e9</font>  <i><font color="#9A1900">/* U+00E9 LATIN SMALL LETTER E WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_ecircumflex                   <font color="#993399">0x00ea</font>  <i><font color="#9A1900">/* U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_ediaeresis                    <font color="#993399">0x00eb</font>  <i><font color="#9A1900">/* U+00EB LATIN SMALL LETTER E WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_igrave                        <font color="#993399">0x00ec</font>  <i><font color="#9A1900">/* U+00EC LATIN SMALL LETTER I WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_iacute                        <font color="#993399">0x00ed</font>  <i><font color="#9A1900">/* U+00ED LATIN SMALL LETTER I WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_icircumflex                   <font color="#993399">0x00ee</font>  <i><font color="#9A1900">/* U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_idiaeresis                    <font color="#993399">0x00ef</font>  <i><font color="#9A1900">/* U+00EF LATIN SMALL LETTER I WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_eth                           <font color="#993399">0x00f0</font>  <i><font color="#9A1900">/* U+00F0 LATIN SMALL LETTER ETH */</font></i>
<b><font color="#000080">#define</font></b> XK_ntilde                        <font color="#993399">0x00f1</font>  <i><font color="#9A1900">/* U+00F1 LATIN SMALL LETTER N WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_ograve                        <font color="#993399">0x00f2</font>  <i><font color="#9A1900">/* U+00F2 LATIN SMALL LETTER O WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_oacute                        <font color="#993399">0x00f3</font>  <i><font color="#9A1900">/* U+00F3 LATIN SMALL LETTER O WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_ocircumflex                   <font color="#993399">0x00f4</font>  <i><font color="#9A1900">/* U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_otilde                        <font color="#993399">0x00f5</font>  <i><font color="#9A1900">/* U+00F5 LATIN SMALL LETTER O WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_odiaeresis                    <font color="#993399">0x00f6</font>  <i><font color="#9A1900">/* U+00F6 LATIN SMALL LETTER O WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_division                      <font color="#993399">0x00f7</font>  <i><font color="#9A1900">/* U+00F7 DIVISION SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_oslash                        <font color="#993399">0x00f8</font>  <i><font color="#9A1900">/* U+00F8 LATIN SMALL LETTER O WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_ooblique                      <font color="#993399">0x00f8</font>  <i><font color="#9A1900">/* U+00F8 LATIN SMALL LETTER O WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_ugrave                        <font color="#993399">0x00f9</font>  <i><font color="#9A1900">/* U+00F9 LATIN SMALL LETTER U WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_uacute                        <font color="#993399">0x00fa</font>  <i><font color="#9A1900">/* U+00FA LATIN SMALL LETTER U WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_ucircumflex                   <font color="#993399">0x00fb</font>  <i><font color="#9A1900">/* U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_udiaeresis                    <font color="#993399">0x00fc</font>  <i><font color="#9A1900">/* U+00FC LATIN SMALL LETTER U WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_yacute                        <font color="#993399">0x00fd</font>  <i><font color="#9A1900">/* U+00FD LATIN SMALL LETTER Y WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_thorn                         <font color="#993399">0x00fe</font>  <i><font color="#9A1900">/* U+00FE LATIN SMALL LETTER THORN */</font></i>
<b><font color="#000080">#define</font></b> XK_ydiaeresis                    <font color="#993399">0x00ff</font>  <i><font color="#9A1900">/* U+00FF LATIN SMALL LETTER Y WITH DIAERESIS */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_LATIN1 */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Latin 2</font></i>
<i><font color="#9A1900"> * Byte 3 = 1</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_LATIN2
<b><font color="#000080">#define</font></b> XK_Aogonek                       <font color="#993399">0x01a1</font>  <i><font color="#9A1900">/* U+0104 LATIN CAPITAL LETTER A WITH OGONEK */</font></i>
<b><font color="#000080">#define</font></b> XK_breve                         <font color="#993399">0x01a2</font>  <i><font color="#9A1900">/* U+02D8 BREVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Lstroke                       <font color="#993399">0x01a3</font>  <i><font color="#9A1900">/* U+0141 LATIN CAPITAL LETTER L WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Lcaron                        <font color="#993399">0x01a5</font>  <i><font color="#9A1900">/* U+013D LATIN CAPITAL LETTER L WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Sacute                        <font color="#993399">0x01a6</font>  <i><font color="#9A1900">/* U+015A LATIN CAPITAL LETTER S WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Scaron                        <font color="#993399">0x01a9</font>  <i><font color="#9A1900">/* U+0160 LATIN CAPITAL LETTER S WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Scedilla                      <font color="#993399">0x01aa</font>  <i><font color="#9A1900">/* U+015E LATIN CAPITAL LETTER S WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_Tcaron                        <font color="#993399">0x01ab</font>  <i><font color="#9A1900">/* U+0164 LATIN CAPITAL LETTER T WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Zacute                        <font color="#993399">0x01ac</font>  <i><font color="#9A1900">/* U+0179 LATIN CAPITAL LETTER Z WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Zcaron                        <font color="#993399">0x01ae</font>  <i><font color="#9A1900">/* U+017D LATIN CAPITAL LETTER Z WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Zabovedot                     <font color="#993399">0x01af</font>  <i><font color="#9A1900">/* U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_aogonek                       <font color="#993399">0x01b1</font>  <i><font color="#9A1900">/* U+0105 LATIN SMALL LETTER A WITH OGONEK */</font></i>
<b><font color="#000080">#define</font></b> XK_ogonek                        <font color="#993399">0x01b2</font>  <i><font color="#9A1900">/* U+02DB OGONEK */</font></i>
<b><font color="#000080">#define</font></b> XK_lstroke                       <font color="#993399">0x01b3</font>  <i><font color="#9A1900">/* U+0142 LATIN SMALL LETTER L WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_lcaron                        <font color="#993399">0x01b5</font>  <i><font color="#9A1900">/* U+013E LATIN SMALL LETTER L WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_sacute                        <font color="#993399">0x01b6</font>  <i><font color="#9A1900">/* U+015B LATIN SMALL LETTER S WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_caron                         <font color="#993399">0x01b7</font>  <i><font color="#9A1900">/* U+02C7 CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_scaron                        <font color="#993399">0x01b9</font>  <i><font color="#9A1900">/* U+0161 LATIN SMALL LETTER S WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_scedilla                      <font color="#993399">0x01ba</font>  <i><font color="#9A1900">/* U+015F LATIN SMALL LETTER S WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_tcaron                        <font color="#993399">0x01bb</font>  <i><font color="#9A1900">/* U+0165 LATIN SMALL LETTER T WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_zacute                        <font color="#993399">0x01bc</font>  <i><font color="#9A1900">/* U+017A LATIN SMALL LETTER Z WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_doubleacute                   <font color="#993399">0x01bd</font>  <i><font color="#9A1900">/* U+02DD DOUBLE ACUTE ACCENT */</font></i>
<b><font color="#000080">#define</font></b> XK_zcaron                        <font color="#993399">0x01be</font>  <i><font color="#9A1900">/* U+017E LATIN SMALL LETTER Z WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_zabovedot                     <font color="#993399">0x01bf</font>  <i><font color="#9A1900">/* U+017C LATIN SMALL LETTER Z WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Racute                        <font color="#993399">0x01c0</font>  <i><font color="#9A1900">/* U+0154 LATIN CAPITAL LETTER R WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Abreve                        <font color="#993399">0x01c3</font>  <i><font color="#9A1900">/* U+0102 LATIN CAPITAL LETTER A WITH BREVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Lacute                        <font color="#993399">0x01c5</font>  <i><font color="#9A1900">/* U+0139 LATIN CAPITAL LETTER L WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cacute                        <font color="#993399">0x01c6</font>  <i><font color="#9A1900">/* U+0106 LATIN CAPITAL LETTER C WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ccaron                        <font color="#993399">0x01c8</font>  <i><font color="#9A1900">/* U+010C LATIN CAPITAL LETTER C WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Eogonek                       <font color="#993399">0x01ca</font>  <i><font color="#9A1900">/* U+0118 LATIN CAPITAL LETTER E WITH OGONEK */</font></i>
<b><font color="#000080">#define</font></b> XK_Ecaron                        <font color="#993399">0x01cc</font>  <i><font color="#9A1900">/* U+011A LATIN CAPITAL LETTER E WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Dcaron                        <font color="#993399">0x01cf</font>  <i><font color="#9A1900">/* U+010E LATIN CAPITAL LETTER D WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Dstroke                       <font color="#993399">0x01d0</font>  <i><font color="#9A1900">/* U+0110 LATIN CAPITAL LETTER D WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Nacute                        <font color="#993399">0x01d1</font>  <i><font color="#9A1900">/* U+0143 LATIN CAPITAL LETTER N WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ncaron                        <font color="#993399">0x01d2</font>  <i><font color="#9A1900">/* U+0147 LATIN CAPITAL LETTER N WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Odoubleacute                  <font color="#993399">0x01d5</font>  <i><font color="#9A1900">/* U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Rcaron                        <font color="#993399">0x01d8</font>  <i><font color="#9A1900">/* U+0158 LATIN CAPITAL LETTER R WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Uring                         <font color="#993399">0x01d9</font>  <i><font color="#9A1900">/* U+016E LATIN CAPITAL LETTER U WITH RING ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Udoubleacute                  <font color="#993399">0x01db</font>  <i><font color="#9A1900">/* U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Tcedilla                      <font color="#993399">0x01de</font>  <i><font color="#9A1900">/* U+0162 LATIN CAPITAL LETTER T WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_racute                        <font color="#993399">0x01e0</font>  <i><font color="#9A1900">/* U+0155 LATIN SMALL LETTER R WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_abreve                        <font color="#993399">0x01e3</font>  <i><font color="#9A1900">/* U+0103 LATIN SMALL LETTER A WITH BREVE */</font></i>
<b><font color="#000080">#define</font></b> XK_lacute                        <font color="#993399">0x01e5</font>  <i><font color="#9A1900">/* U+013A LATIN SMALL LETTER L WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_cacute                        <font color="#993399">0x01e6</font>  <i><font color="#9A1900">/* U+0107 LATIN SMALL LETTER C WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_ccaron                        <font color="#993399">0x01e8</font>  <i><font color="#9A1900">/* U+010D LATIN SMALL LETTER C WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_eogonek                       <font color="#993399">0x01ea</font>  <i><font color="#9A1900">/* U+0119 LATIN SMALL LETTER E WITH OGONEK */</font></i>
<b><font color="#000080">#define</font></b> XK_ecaron                        <font color="#993399">0x01ec</font>  <i><font color="#9A1900">/* U+011B LATIN SMALL LETTER E WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_dcaron                        <font color="#993399">0x01ef</font>  <i><font color="#9A1900">/* U+010F LATIN SMALL LETTER D WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_dstroke                       <font color="#993399">0x01f0</font>  <i><font color="#9A1900">/* U+0111 LATIN SMALL LETTER D WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_nacute                        <font color="#993399">0x01f1</font>  <i><font color="#9A1900">/* U+0144 LATIN SMALL LETTER N WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_ncaron                        <font color="#993399">0x01f2</font>  <i><font color="#9A1900">/* U+0148 LATIN SMALL LETTER N WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_odoubleacute                  <font color="#993399">0x01f5</font>  <i><font color="#9A1900">/* U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_udoubleacute                  <font color="#993399">0x01fb</font>  <i><font color="#9A1900">/* U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_rcaron                        <font color="#993399">0x01f8</font>  <i><font color="#9A1900">/* U+0159 LATIN SMALL LETTER R WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_uring                         <font color="#993399">0x01f9</font>  <i><font color="#9A1900">/* U+016F LATIN SMALL LETTER U WITH RING ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_tcedilla                      <font color="#993399">0x01fe</font>  <i><font color="#9A1900">/* U+0163 LATIN SMALL LETTER T WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_abovedot                      <font color="#993399">0x01ff</font>  <i><font color="#9A1900">/* U+02D9 DOT ABOVE */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_LATIN2 */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Latin 3</font></i>
<i><font color="#9A1900"> * Byte 3 = 2</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_LATIN3
<b><font color="#000080">#define</font></b> XK_Hstroke                       <font color="#993399">0x02a1</font>  <i><font color="#9A1900">/* U+0126 LATIN CAPITAL LETTER H WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Hcircumflex                   <font color="#993399">0x02a6</font>  <i><font color="#9A1900">/* U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Iabovedot                     <font color="#993399">0x02a9</font>  <i><font color="#9A1900">/* U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Gbreve                        <font color="#993399">0x02ab</font>  <i><font color="#9A1900">/* U+011E LATIN CAPITAL LETTER G WITH BREVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Jcircumflex                   <font color="#993399">0x02ac</font>  <i><font color="#9A1900">/* U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_hstroke                       <font color="#993399">0x02b1</font>  <i><font color="#9A1900">/* U+0127 LATIN SMALL LETTER H WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_hcircumflex                   <font color="#993399">0x02b6</font>  <i><font color="#9A1900">/* U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_idotless                      <font color="#993399">0x02b9</font>  <i><font color="#9A1900">/* U+0131 LATIN SMALL LETTER DOTLESS I */</font></i>
<b><font color="#000080">#define</font></b> XK_gbreve                        <font color="#993399">0x02bb</font>  <i><font color="#9A1900">/* U+011F LATIN SMALL LETTER G WITH BREVE */</font></i>
<b><font color="#000080">#define</font></b> XK_jcircumflex                   <font color="#993399">0x02bc</font>  <i><font color="#9A1900">/* U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Cabovedot                     <font color="#993399">0x02c5</font>  <i><font color="#9A1900">/* U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ccircumflex                   <font color="#993399">0x02c6</font>  <i><font color="#9A1900">/* U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Gabovedot                     <font color="#993399">0x02d5</font>  <i><font color="#9A1900">/* U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Gcircumflex                   <font color="#993399">0x02d8</font>  <i><font color="#9A1900">/* U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Ubreve                        <font color="#993399">0x02dd</font>  <i><font color="#9A1900">/* U+016C LATIN CAPITAL LETTER U WITH BREVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Scircumflex                   <font color="#993399">0x02de</font>  <i><font color="#9A1900">/* U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_cabovedot                     <font color="#993399">0x02e5</font>  <i><font color="#9A1900">/* U+010B LATIN SMALL LETTER C WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ccircumflex                   <font color="#993399">0x02e6</font>  <i><font color="#9A1900">/* U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_gabovedot                     <font color="#993399">0x02f5</font>  <i><font color="#9A1900">/* U+0121 LATIN SMALL LETTER G WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_gcircumflex                   <font color="#993399">0x02f8</font>  <i><font color="#9A1900">/* U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_ubreve                        <font color="#993399">0x02fd</font>  <i><font color="#9A1900">/* U+016D LATIN SMALL LETTER U WITH BREVE */</font></i>
<b><font color="#000080">#define</font></b> XK_scircumflex                   <font color="#993399">0x02fe</font>  <i><font color="#9A1900">/* U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_LATIN3 */</font></i>


<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Latin 4</font></i>
<i><font color="#9A1900"> * Byte 3 = 3</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_LATIN4
<b><font color="#000080">#define</font></b> XK_kra                           <font color="#993399">0x03a2</font>  <i><font color="#9A1900">/* U+0138 LATIN SMALL LETTER KRA */</font></i>
<b><font color="#000080">#define</font></b> XK_kappa                         <font color="#993399">0x03a2</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Rcedilla                      <font color="#993399">0x03a3</font>  <i><font color="#9A1900">/* U+0156 LATIN CAPITAL LETTER R WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_Itilde                        <font color="#993399">0x03a5</font>  <i><font color="#9A1900">/* U+0128 LATIN CAPITAL LETTER I WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Lcedilla                      <font color="#993399">0x03a6</font>  <i><font color="#9A1900">/* U+013B LATIN CAPITAL LETTER L WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_Emacron                       <font color="#993399">0x03aa</font>  <i><font color="#9A1900">/* U+0112 LATIN CAPITAL LETTER E WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_Gcedilla                      <font color="#993399">0x03ab</font>  <i><font color="#9A1900">/* U+0122 LATIN CAPITAL LETTER G WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_Tslash                        <font color="#993399">0x03ac</font>  <i><font color="#9A1900">/* U+0166 LATIN CAPITAL LETTER T WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_rcedilla                      <font color="#993399">0x03b3</font>  <i><font color="#9A1900">/* U+0157 LATIN SMALL LETTER R WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_itilde                        <font color="#993399">0x03b5</font>  <i><font color="#9A1900">/* U+0129 LATIN SMALL LETTER I WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_lcedilla                      <font color="#993399">0x03b6</font>  <i><font color="#9A1900">/* U+013C LATIN SMALL LETTER L WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_emacron                       <font color="#993399">0x03ba</font>  <i><font color="#9A1900">/* U+0113 LATIN SMALL LETTER E WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_gcedilla                      <font color="#993399">0x03bb</font>  <i><font color="#9A1900">/* U+0123 LATIN SMALL LETTER G WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_tslash                        <font color="#993399">0x03bc</font>  <i><font color="#9A1900">/* U+0167 LATIN SMALL LETTER T WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_ENG                           <font color="#993399">0x03bd</font>  <i><font color="#9A1900">/* U+014A LATIN CAPITAL LETTER ENG */</font></i>
<b><font color="#000080">#define</font></b> XK_eng                           <font color="#993399">0x03bf</font>  <i><font color="#9A1900">/* U+014B LATIN SMALL LETTER ENG */</font></i>
<b><font color="#000080">#define</font></b> XK_Amacron                       <font color="#993399">0x03c0</font>  <i><font color="#9A1900">/* U+0100 LATIN CAPITAL LETTER A WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_Iogonek                       <font color="#993399">0x03c7</font>  <i><font color="#9A1900">/* U+012E LATIN CAPITAL LETTER I WITH OGONEK */</font></i>
<b><font color="#000080">#define</font></b> XK_Eabovedot                     <font color="#993399">0x03cc</font>  <i><font color="#9A1900">/* U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Imacron                       <font color="#993399">0x03cf</font>  <i><font color="#9A1900">/* U+012A LATIN CAPITAL LETTER I WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_Ncedilla                      <font color="#993399">0x03d1</font>  <i><font color="#9A1900">/* U+0145 LATIN CAPITAL LETTER N WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_Omacron                       <font color="#993399">0x03d2</font>  <i><font color="#9A1900">/* U+014C LATIN CAPITAL LETTER O WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_Kcedilla                      <font color="#993399">0x03d3</font>  <i><font color="#9A1900">/* U+0136 LATIN CAPITAL LETTER K WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_Uogonek                       <font color="#993399">0x03d9</font>  <i><font color="#9A1900">/* U+0172 LATIN CAPITAL LETTER U WITH OGONEK */</font></i>
<b><font color="#000080">#define</font></b> XK_Utilde                        <font color="#993399">0x03dd</font>  <i><font color="#9A1900">/* U+0168 LATIN CAPITAL LETTER U WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Umacron                       <font color="#993399">0x03de</font>  <i><font color="#9A1900">/* U+016A LATIN CAPITAL LETTER U WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_amacron                       <font color="#993399">0x03e0</font>  <i><font color="#9A1900">/* U+0101 LATIN SMALL LETTER A WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_iogonek                       <font color="#993399">0x03e7</font>  <i><font color="#9A1900">/* U+012F LATIN SMALL LETTER I WITH OGONEK */</font></i>
<b><font color="#000080">#define</font></b> XK_eabovedot                     <font color="#993399">0x03ec</font>  <i><font color="#9A1900">/* U+0117 LATIN SMALL LETTER E WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_imacron                       <font color="#993399">0x03ef</font>  <i><font color="#9A1900">/* U+012B LATIN SMALL LETTER I WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_ncedilla                      <font color="#993399">0x03f1</font>  <i><font color="#9A1900">/* U+0146 LATIN SMALL LETTER N WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_omacron                       <font color="#993399">0x03f2</font>  <i><font color="#9A1900">/* U+014D LATIN SMALL LETTER O WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_kcedilla                      <font color="#993399">0x03f3</font>  <i><font color="#9A1900">/* U+0137 LATIN SMALL LETTER K WITH CEDILLA */</font></i>
<b><font color="#000080">#define</font></b> XK_uogonek                       <font color="#993399">0x03f9</font>  <i><font color="#9A1900">/* U+0173 LATIN SMALL LETTER U WITH OGONEK */</font></i>
<b><font color="#000080">#define</font></b> XK_utilde                        <font color="#993399">0x03fd</font>  <i><font color="#9A1900">/* U+0169 LATIN SMALL LETTER U WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_umacron                       <font color="#993399">0x03fe</font>  <i><font color="#9A1900">/* U+016B LATIN SMALL LETTER U WITH MACRON */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_LATIN4 */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Latin 8</font></i>
<i><font color="#9A1900"> */</font></i>
<b><font color="#000080">#ifdef</font></b> XK_LATIN8
<b><font color="#000080">#define</font></b> XK_Babovedot                  <font color="#993399">0x1001e02</font>  <i><font color="#9A1900">/* U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_babovedot                  <font color="#993399">0x1001e03</font>  <i><font color="#9A1900">/* U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Dabovedot                  <font color="#993399">0x1001e0a</font>  <i><font color="#9A1900">/* U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Wgrave                     <font color="#993399">0x1001e80</font>  <i><font color="#9A1900">/* U+1E80 LATIN CAPITAL LETTER W WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Wacute                     <font color="#993399">0x1001e82</font>  <i><font color="#9A1900">/* U+1E82 LATIN CAPITAL LETTER W WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_dabovedot                  <font color="#993399">0x1001e0b</font>  <i><font color="#9A1900">/* U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ygrave                     <font color="#993399">0x1001ef2</font>  <i><font color="#9A1900">/* U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Fabovedot                  <font color="#993399">0x1001e1e</font>  <i><font color="#9A1900">/* U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_fabovedot                  <font color="#993399">0x1001e1f</font>  <i><font color="#9A1900">/* U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Mabovedot                  <font color="#993399">0x1001e40</font>  <i><font color="#9A1900">/* U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_mabovedot                  <font color="#993399">0x1001e41</font>  <i><font color="#9A1900">/* U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Pabovedot                  <font color="#993399">0x1001e56</font>  <i><font color="#9A1900">/* U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_wgrave                     <font color="#993399">0x1001e81</font>  <i><font color="#9A1900">/* U+1E81 LATIN SMALL LETTER W WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_pabovedot                  <font color="#993399">0x1001e57</font>  <i><font color="#9A1900">/* U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_wacute                     <font color="#993399">0x1001e83</font>  <i><font color="#9A1900">/* U+1E83 LATIN SMALL LETTER W WITH ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Sabovedot                  <font color="#993399">0x1001e60</font>  <i><font color="#9A1900">/* U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ygrave                     <font color="#993399">0x1001ef3</font>  <i><font color="#9A1900">/* U+1EF3 LATIN SMALL LETTER Y WITH GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Wdiaeresis                 <font color="#993399">0x1001e84</font>  <i><font color="#9A1900">/* U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_wdiaeresis                 <font color="#993399">0x1001e85</font>  <i><font color="#9A1900">/* U+1E85 LATIN SMALL LETTER W WITH DIAERESIS */</font></i>
<b><font color="#000080">#define</font></b> XK_sabovedot                  <font color="#993399">0x1001e61</font>  <i><font color="#9A1900">/* U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Wcircumflex                <font color="#993399">0x1000174</font>  <i><font color="#9A1900">/* U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_Tabovedot                  <font color="#993399">0x1001e6a</font>  <i><font color="#9A1900">/* U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ycircumflex                <font color="#993399">0x1000176</font>  <i><font color="#9A1900">/* U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_wcircumflex                <font color="#993399">0x1000175</font>  <i><font color="#9A1900">/* U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#define</font></b> XK_tabovedot                  <font color="#993399">0x1001e6b</font>  <i><font color="#9A1900">/* U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ycircumflex                <font color="#993399">0x1000177</font>  <i><font color="#9A1900">/* U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_LATIN8 */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Latin 9</font></i>
<i><font color="#9A1900"> * Byte 3 = 0x13</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_LATIN9
<b><font color="#000080">#define</font></b> XK_OE                            <font color="#993399">0x13bc</font>  <i><font color="#9A1900">/* U+0152 LATIN CAPITAL LIGATURE OE */</font></i>
<b><font color="#000080">#define</font></b> XK_oe                            <font color="#993399">0x13bd</font>  <i><font color="#9A1900">/* U+0153 LATIN SMALL LIGATURE OE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ydiaeresis                    <font color="#993399">0x13be</font>  <i><font color="#9A1900">/* U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_LATIN9 */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Katakana</font></i>
<i><font color="#9A1900"> * Byte 3 = 4</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_KATAKANA
<b><font color="#000080">#define</font></b> XK_overline                      <font color="#993399">0x047e</font>  <i><font color="#9A1900">/* U+203E OVERLINE */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_fullstop                 <font color="#993399">0x04a1</font>  <i><font color="#9A1900">/* U+3002 IDEOGRAPHIC FULL STOP */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_openingbracket           <font color="#993399">0x04a2</font>  <i><font color="#9A1900">/* U+300C LEFT CORNER BRACKET */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_closingbracket           <font color="#993399">0x04a3</font>  <i><font color="#9A1900">/* U+300D RIGHT CORNER BRACKET */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_comma                    <font color="#993399">0x04a4</font>  <i><font color="#9A1900">/* U+3001 IDEOGRAPHIC COMMA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_conjunctive              <font color="#993399">0x04a5</font>  <i><font color="#9A1900">/* U+30FB KATAKANA MIDDLE DOT */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_middledot                <font color="#993399">0x04a5</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_WO                       <font color="#993399">0x04a6</font>  <i><font color="#9A1900">/* U+30F2 KATAKANA LETTER WO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_a                        <font color="#993399">0x04a7</font>  <i><font color="#9A1900">/* U+30A1 KATAKANA LETTER SMALL A */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_i                        <font color="#993399">0x04a8</font>  <i><font color="#9A1900">/* U+30A3 KATAKANA LETTER SMALL I */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_u                        <font color="#993399">0x04a9</font>  <i><font color="#9A1900">/* U+30A5 KATAKANA LETTER SMALL U */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_e                        <font color="#993399">0x04aa</font>  <i><font color="#9A1900">/* U+30A7 KATAKANA LETTER SMALL E */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_o                        <font color="#993399">0x04ab</font>  <i><font color="#9A1900">/* U+30A9 KATAKANA LETTER SMALL O */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_ya                       <font color="#993399">0x04ac</font>  <i><font color="#9A1900">/* U+30E3 KATAKANA LETTER SMALL YA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_yu                       <font color="#993399">0x04ad</font>  <i><font color="#9A1900">/* U+30E5 KATAKANA LETTER SMALL YU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_yo                       <font color="#993399">0x04ae</font>  <i><font color="#9A1900">/* U+30E7 KATAKANA LETTER SMALL YO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_tsu                      <font color="#993399">0x04af</font>  <i><font color="#9A1900">/* U+30C3 KATAKANA LETTER SMALL TU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_tu                       <font color="#993399">0x04af</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_prolongedsound                <font color="#993399">0x04b0</font>  <i><font color="#9A1900">/* U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_A                        <font color="#993399">0x04b1</font>  <i><font color="#9A1900">/* U+30A2 KATAKANA LETTER A */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_I                        <font color="#993399">0x04b2</font>  <i><font color="#9A1900">/* U+30A4 KATAKANA LETTER I */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_U                        <font color="#993399">0x04b3</font>  <i><font color="#9A1900">/* U+30A6 KATAKANA LETTER U */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_E                        <font color="#993399">0x04b4</font>  <i><font color="#9A1900">/* U+30A8 KATAKANA LETTER E */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_O                        <font color="#993399">0x04b5</font>  <i><font color="#9A1900">/* U+30AA KATAKANA LETTER O */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_KA                       <font color="#993399">0x04b6</font>  <i><font color="#9A1900">/* U+30AB KATAKANA LETTER KA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_KI                       <font color="#993399">0x04b7</font>  <i><font color="#9A1900">/* U+30AD KATAKANA LETTER KI */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_KU                       <font color="#993399">0x04b8</font>  <i><font color="#9A1900">/* U+30AF KATAKANA LETTER KU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_KE                       <font color="#993399">0x04b9</font>  <i><font color="#9A1900">/* U+30B1 KATAKANA LETTER KE */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_KO                       <font color="#993399">0x04ba</font>  <i><font color="#9A1900">/* U+30B3 KATAKANA LETTER KO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_SA                       <font color="#993399">0x04bb</font>  <i><font color="#9A1900">/* U+30B5 KATAKANA LETTER SA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_SHI                      <font color="#993399">0x04bc</font>  <i><font color="#9A1900">/* U+30B7 KATAKANA LETTER SI */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_SU                       <font color="#993399">0x04bd</font>  <i><font color="#9A1900">/* U+30B9 KATAKANA LETTER SU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_SE                       <font color="#993399">0x04be</font>  <i><font color="#9A1900">/* U+30BB KATAKANA LETTER SE */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_SO                       <font color="#993399">0x04bf</font>  <i><font color="#9A1900">/* U+30BD KATAKANA LETTER SO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_TA                       <font color="#993399">0x04c0</font>  <i><font color="#9A1900">/* U+30BF KATAKANA LETTER TA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_CHI                      <font color="#993399">0x04c1</font>  <i><font color="#9A1900">/* U+30C1 KATAKANA LETTER TI */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_TI                       <font color="#993399">0x04c1</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_TSU                      <font color="#993399">0x04c2</font>  <i><font color="#9A1900">/* U+30C4 KATAKANA LETTER TU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_TU                       <font color="#993399">0x04c2</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_TE                       <font color="#993399">0x04c3</font>  <i><font color="#9A1900">/* U+30C6 KATAKANA LETTER TE */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_TO                       <font color="#993399">0x04c4</font>  <i><font color="#9A1900">/* U+30C8 KATAKANA LETTER TO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_NA                       <font color="#993399">0x04c5</font>  <i><font color="#9A1900">/* U+30CA KATAKANA LETTER NA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_NI                       <font color="#993399">0x04c6</font>  <i><font color="#9A1900">/* U+30CB KATAKANA LETTER NI */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_NU                       <font color="#993399">0x04c7</font>  <i><font color="#9A1900">/* U+30CC KATAKANA LETTER NU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_NE                       <font color="#993399">0x04c8</font>  <i><font color="#9A1900">/* U+30CD KATAKANA LETTER NE */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_NO                       <font color="#993399">0x04c9</font>  <i><font color="#9A1900">/* U+30CE KATAKANA LETTER NO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_HA                       <font color="#993399">0x04ca</font>  <i><font color="#9A1900">/* U+30CF KATAKANA LETTER HA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_HI                       <font color="#993399">0x04cb</font>  <i><font color="#9A1900">/* U+30D2 KATAKANA LETTER HI */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_FU                       <font color="#993399">0x04cc</font>  <i><font color="#9A1900">/* U+30D5 KATAKANA LETTER HU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_HU                       <font color="#993399">0x04cc</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_HE                       <font color="#993399">0x04cd</font>  <i><font color="#9A1900">/* U+30D8 KATAKANA LETTER HE */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_HO                       <font color="#993399">0x04ce</font>  <i><font color="#9A1900">/* U+30DB KATAKANA LETTER HO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_MA                       <font color="#993399">0x04cf</font>  <i><font color="#9A1900">/* U+30DE KATAKANA LETTER MA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_MI                       <font color="#993399">0x04d0</font>  <i><font color="#9A1900">/* U+30DF KATAKANA LETTER MI */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_MU                       <font color="#993399">0x04d1</font>  <i><font color="#9A1900">/* U+30E0 KATAKANA LETTER MU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_ME                       <font color="#993399">0x04d2</font>  <i><font color="#9A1900">/* U+30E1 KATAKANA LETTER ME */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_MO                       <font color="#993399">0x04d3</font>  <i><font color="#9A1900">/* U+30E2 KATAKANA LETTER MO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_YA                       <font color="#993399">0x04d4</font>  <i><font color="#9A1900">/* U+30E4 KATAKANA LETTER YA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_YU                       <font color="#993399">0x04d5</font>  <i><font color="#9A1900">/* U+30E6 KATAKANA LETTER YU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_YO                       <font color="#993399">0x04d6</font>  <i><font color="#9A1900">/* U+30E8 KATAKANA LETTER YO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_RA                       <font color="#993399">0x04d7</font>  <i><font color="#9A1900">/* U+30E9 KATAKANA LETTER RA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_RI                       <font color="#993399">0x04d8</font>  <i><font color="#9A1900">/* U+30EA KATAKANA LETTER RI */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_RU                       <font color="#993399">0x04d9</font>  <i><font color="#9A1900">/* U+30EB KATAKANA LETTER RU */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_RE                       <font color="#993399">0x04da</font>  <i><font color="#9A1900">/* U+30EC KATAKANA LETTER RE */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_RO                       <font color="#993399">0x04db</font>  <i><font color="#9A1900">/* U+30ED KATAKANA LETTER RO */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_WA                       <font color="#993399">0x04dc</font>  <i><font color="#9A1900">/* U+30EF KATAKANA LETTER WA */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_N                        <font color="#993399">0x04dd</font>  <i><font color="#9A1900">/* U+30F3 KATAKANA LETTER N */</font></i>
<b><font color="#000080">#define</font></b> XK_voicedsound                   <font color="#993399">0x04de</font>  <i><font color="#9A1900">/* U+309B KATAKANA-HIRAGANA VOICED SOUND MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_semivoicedsound               <font color="#993399">0x04df</font>  <i><font color="#9A1900">/* U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_kana_switch                   <font color="#993399">0xff7e</font>  <i><font color="#9A1900">/* Alias for mode_switch */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_KATAKANA */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Arabic</font></i>
<i><font color="#9A1900"> * Byte 3 = 5</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_ARABIC
<b><font color="#000080">#define</font></b> XK_Farsi_0                    <font color="#993399">0x10006f0</font>  <i><font color="#9A1900">/* U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_1                    <font color="#993399">0x10006f1</font>  <i><font color="#9A1900">/* U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_2                    <font color="#993399">0x10006f2</font>  <i><font color="#9A1900">/* U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_3                    <font color="#993399">0x10006f3</font>  <i><font color="#9A1900">/* U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_4                    <font color="#993399">0x10006f4</font>  <i><font color="#9A1900">/* U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_5                    <font color="#993399">0x10006f5</font>  <i><font color="#9A1900">/* U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_6                    <font color="#993399">0x10006f6</font>  <i><font color="#9A1900">/* U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_7                    <font color="#993399">0x10006f7</font>  <i><font color="#9A1900">/* U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_8                    <font color="#993399">0x10006f8</font>  <i><font color="#9A1900">/* U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_9                    <font color="#993399">0x10006f9</font>  <i><font color="#9A1900">/* U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_percent             <font color="#993399">0x100066a</font>  <i><font color="#9A1900">/* U+066A ARABIC PERCENT SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_superscript_alef    <font color="#993399">0x1000670</font>  <i><font color="#9A1900">/* U+0670 ARABIC LETTER SUPERSCRIPT ALEF */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_tteh                <font color="#993399">0x1000679</font>  <i><font color="#9A1900">/* U+0679 ARABIC LETTER TTEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_peh                 <font color="#993399">0x100067e</font>  <i><font color="#9A1900">/* U+067E ARABIC LETTER PEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_tcheh               <font color="#993399">0x1000686</font>  <i><font color="#9A1900">/* U+0686 ARABIC LETTER TCHEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_ddal                <font color="#993399">0x1000688</font>  <i><font color="#9A1900">/* U+0688 ARABIC LETTER DDAL */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_rreh                <font color="#993399">0x1000691</font>  <i><font color="#9A1900">/* U+0691 ARABIC LETTER RREH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_comma                  <font color="#993399">0x05ac</font>  <i><font color="#9A1900">/* U+060C ARABIC COMMA */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_fullstop            <font color="#993399">0x10006d4</font>  <i><font color="#9A1900">/* U+06D4 ARABIC FULL STOP */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_0                   <font color="#993399">0x1000660</font>  <i><font color="#9A1900">/* U+0660 ARABIC-INDIC DIGIT ZERO */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_1                   <font color="#993399">0x1000661</font>  <i><font color="#9A1900">/* U+0661 ARABIC-INDIC DIGIT ONE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_2                   <font color="#993399">0x1000662</font>  <i><font color="#9A1900">/* U+0662 ARABIC-INDIC DIGIT TWO */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_3                   <font color="#993399">0x1000663</font>  <i><font color="#9A1900">/* U+0663 ARABIC-INDIC DIGIT THREE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_4                   <font color="#993399">0x1000664</font>  <i><font color="#9A1900">/* U+0664 ARABIC-INDIC DIGIT FOUR */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_5                   <font color="#993399">0x1000665</font>  <i><font color="#9A1900">/* U+0665 ARABIC-INDIC DIGIT FIVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_6                   <font color="#993399">0x1000666</font>  <i><font color="#9A1900">/* U+0666 ARABIC-INDIC DIGIT SIX */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_7                   <font color="#993399">0x1000667</font>  <i><font color="#9A1900">/* U+0667 ARABIC-INDIC DIGIT SEVEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_8                   <font color="#993399">0x1000668</font>  <i><font color="#9A1900">/* U+0668 ARABIC-INDIC DIGIT EIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_9                   <font color="#993399">0x1000669</font>  <i><font color="#9A1900">/* U+0669 ARABIC-INDIC DIGIT NINE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_semicolon              <font color="#993399">0x05bb</font>  <i><font color="#9A1900">/* U+061B ARABIC SEMICOLON */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_question_mark          <font color="#993399">0x05bf</font>  <i><font color="#9A1900">/* U+061F ARABIC QUESTION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_hamza                  <font color="#993399">0x05c1</font>  <i><font color="#9A1900">/* U+0621 ARABIC LETTER HAMZA */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_maddaonalef            <font color="#993399">0x05c2</font>  <i><font color="#9A1900">/* U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_hamzaonalef            <font color="#993399">0x05c3</font>  <i><font color="#9A1900">/* U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_hamzaonwaw             <font color="#993399">0x05c4</font>  <i><font color="#9A1900">/* U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_hamzaunderalef         <font color="#993399">0x05c5</font>  <i><font color="#9A1900">/* U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_hamzaonyeh             <font color="#993399">0x05c6</font>  <i><font color="#9A1900">/* U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_alef                   <font color="#993399">0x05c7</font>  <i><font color="#9A1900">/* U+0627 ARABIC LETTER ALEF */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_beh                    <font color="#993399">0x05c8</font>  <i><font color="#9A1900">/* U+0628 ARABIC LETTER BEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_tehmarbuta             <font color="#993399">0x05c9</font>  <i><font color="#9A1900">/* U+0629 ARABIC LETTER TEH MARBUTA */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_teh                    <font color="#993399">0x05ca</font>  <i><font color="#9A1900">/* U+062A ARABIC LETTER TEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_theh                   <font color="#993399">0x05cb</font>  <i><font color="#9A1900">/* U+062B ARABIC LETTER THEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_jeem                   <font color="#993399">0x05cc</font>  <i><font color="#9A1900">/* U+062C ARABIC LETTER JEEM */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_hah                    <font color="#993399">0x05cd</font>  <i><font color="#9A1900">/* U+062D ARABIC LETTER HAH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_khah                   <font color="#993399">0x05ce</font>  <i><font color="#9A1900">/* U+062E ARABIC LETTER KHAH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_dal                    <font color="#993399">0x05cf</font>  <i><font color="#9A1900">/* U+062F ARABIC LETTER DAL */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_thal                   <font color="#993399">0x05d0</font>  <i><font color="#9A1900">/* U+0630 ARABIC LETTER THAL */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_ra                     <font color="#993399">0x05d1</font>  <i><font color="#9A1900">/* U+0631 ARABIC LETTER REH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_zain                   <font color="#993399">0x05d2</font>  <i><font color="#9A1900">/* U+0632 ARABIC LETTER ZAIN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_seen                   <font color="#993399">0x05d3</font>  <i><font color="#9A1900">/* U+0633 ARABIC LETTER SEEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_sheen                  <font color="#993399">0x05d4</font>  <i><font color="#9A1900">/* U+0634 ARABIC LETTER SHEEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_sad                    <font color="#993399">0x05d5</font>  <i><font color="#9A1900">/* U+0635 ARABIC LETTER SAD */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_dad                    <font color="#993399">0x05d6</font>  <i><font color="#9A1900">/* U+0636 ARABIC LETTER DAD */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_tah                    <font color="#993399">0x05d7</font>  <i><font color="#9A1900">/* U+0637 ARABIC LETTER TAH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_zah                    <font color="#993399">0x05d8</font>  <i><font color="#9A1900">/* U+0638 ARABIC LETTER ZAH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_ain                    <font color="#993399">0x05d9</font>  <i><font color="#9A1900">/* U+0639 ARABIC LETTER AIN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_ghain                  <font color="#993399">0x05da</font>  <i><font color="#9A1900">/* U+063A ARABIC LETTER GHAIN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_tatweel                <font color="#993399">0x05e0</font>  <i><font color="#9A1900">/* U+0640 ARABIC TATWEEL */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_feh                    <font color="#993399">0x05e1</font>  <i><font color="#9A1900">/* U+0641 ARABIC LETTER FEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_qaf                    <font color="#993399">0x05e2</font>  <i><font color="#9A1900">/* U+0642 ARABIC LETTER QAF */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_kaf                    <font color="#993399">0x05e3</font>  <i><font color="#9A1900">/* U+0643 ARABIC LETTER KAF */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_lam                    <font color="#993399">0x05e4</font>  <i><font color="#9A1900">/* U+0644 ARABIC LETTER LAM */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_meem                   <font color="#993399">0x05e5</font>  <i><font color="#9A1900">/* U+0645 ARABIC LETTER MEEM */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_noon                   <font color="#993399">0x05e6</font>  <i><font color="#9A1900">/* U+0646 ARABIC LETTER NOON */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_ha                     <font color="#993399">0x05e7</font>  <i><font color="#9A1900">/* U+0647 ARABIC LETTER HEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_heh                    <font color="#993399">0x05e7</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_waw                    <font color="#993399">0x05e8</font>  <i><font color="#9A1900">/* U+0648 ARABIC LETTER WAW */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_alefmaksura            <font color="#993399">0x05e9</font>  <i><font color="#9A1900">/* U+0649 ARABIC LETTER ALEF MAKSURA */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_yeh                    <font color="#993399">0x05ea</font>  <i><font color="#9A1900">/* U+064A ARABIC LETTER YEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_fathatan               <font color="#993399">0x05eb</font>  <i><font color="#9A1900">/* U+064B ARABIC FATHATAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_dammatan               <font color="#993399">0x05ec</font>  <i><font color="#9A1900">/* U+064C ARABIC DAMMATAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_kasratan               <font color="#993399">0x05ed</font>  <i><font color="#9A1900">/* U+064D ARABIC KASRATAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_fatha                  <font color="#993399">0x05ee</font>  <i><font color="#9A1900">/* U+064E ARABIC FATHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_damma                  <font color="#993399">0x05ef</font>  <i><font color="#9A1900">/* U+064F ARABIC DAMMA */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_kasra                  <font color="#993399">0x05f0</font>  <i><font color="#9A1900">/* U+0650 ARABIC KASRA */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_shadda                 <font color="#993399">0x05f1</font>  <i><font color="#9A1900">/* U+0651 ARABIC SHADDA */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_sukun                  <font color="#993399">0x05f2</font>  <i><font color="#9A1900">/* U+0652 ARABIC SUKUN */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_madda_above         <font color="#993399">0x1000653</font>  <i><font color="#9A1900">/* U+0653 ARABIC MADDAH ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_hamza_above         <font color="#993399">0x1000654</font>  <i><font color="#9A1900">/* U+0654 ARABIC HAMZA ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_hamza_below         <font color="#993399">0x1000655</font>  <i><font color="#9A1900">/* U+0655 ARABIC HAMZA BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_jeh                 <font color="#993399">0x1000698</font>  <i><font color="#9A1900">/* U+0698 ARABIC LETTER JEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_veh                 <font color="#993399">0x10006a4</font>  <i><font color="#9A1900">/* U+06A4 ARABIC LETTER VEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_keheh               <font color="#993399">0x10006a9</font>  <i><font color="#9A1900">/* U+06A9 ARABIC LETTER KEHEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_gaf                 <font color="#993399">0x10006af</font>  <i><font color="#9A1900">/* U+06AF ARABIC LETTER GAF */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_noon_ghunna         <font color="#993399">0x10006ba</font>  <i><font color="#9A1900">/* U+06BA ARABIC LETTER NOON GHUNNA */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_heh_doachashmee     <font color="#993399">0x10006be</font>  <i><font color="#9A1900">/* U+06BE ARABIC LETTER HEH DOACHASHMEE */</font></i>
<b><font color="#000080">#define</font></b> XK_Farsi_yeh                  <font color="#993399">0x10006cc</font>  <i><font color="#9A1900">/* U+06CC ARABIC LETTER FARSI YEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_farsi_yeh           <font color="#993399">0x10006cc</font>  <i><font color="#9A1900">/* U+06CC ARABIC LETTER FARSI YEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_yeh_baree           <font color="#993399">0x10006d2</font>  <i><font color="#9A1900">/* U+06D2 ARABIC LETTER YEH BARREE */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_heh_goal            <font color="#993399">0x10006c1</font>  <i><font color="#9A1900">/* U+06C1 ARABIC LETTER HEH GOAL */</font></i>
<b><font color="#000080">#define</font></b> XK_Arabic_switch                 <font color="#993399">0xff7e</font>  <i><font color="#9A1900">/* Alias for mode_switch */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_ARABIC */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Cyrillic</font></i>
<i><font color="#9A1900"> * Byte 3 = 6</font></i>
<i><font color="#9A1900"> */</font></i>
<b><font color="#000080">#ifdef</font></b> XK_CYRILLIC
<b><font color="#000080">#define</font></b> XK_Cyrillic_GHE_bar           <font color="#993399">0x1000492</font>  <i><font color="#9A1900">/* U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ghe_bar           <font color="#993399">0x1000493</font>  <i><font color="#9A1900">/* U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ZHE_descender     <font color="#993399">0x1000496</font>  <i><font color="#9A1900">/* U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_zhe_descender     <font color="#993399">0x1000497</font>  <i><font color="#9A1900">/* U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_KA_descender      <font color="#993399">0x100049a</font>  <i><font color="#9A1900">/* U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ka_descender      <font color="#993399">0x100049b</font>  <i><font color="#9A1900">/* U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_KA_vertstroke     <font color="#993399">0x100049c</font>  <i><font color="#9A1900">/* U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ka_vertstroke     <font color="#993399">0x100049d</font>  <i><font color="#9A1900">/* U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_EN_descender      <font color="#993399">0x10004a2</font>  <i><font color="#9A1900">/* U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_en_descender      <font color="#993399">0x10004a3</font>  <i><font color="#9A1900">/* U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_U_straight        <font color="#993399">0x10004ae</font>  <i><font color="#9A1900">/* U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_u_straight        <font color="#993399">0x10004af</font>  <i><font color="#9A1900">/* U+04AF CYRILLIC SMALL LETTER STRAIGHT U */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_U_straight_bar    <font color="#993399">0x10004b0</font>  <i><font color="#9A1900">/* U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_u_straight_bar    <font color="#993399">0x10004b1</font>  <i><font color="#9A1900">/* U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_HA_descender      <font color="#993399">0x10004b2</font>  <i><font color="#9A1900">/* U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ha_descender      <font color="#993399">0x10004b3</font>  <i><font color="#9A1900">/* U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_CHE_descender     <font color="#993399">0x10004b6</font>  <i><font color="#9A1900">/* U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_che_descender     <font color="#993399">0x10004b7</font>  <i><font color="#9A1900">/* U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_CHE_vertstroke    <font color="#993399">0x10004b8</font>  <i><font color="#9A1900">/* U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_che_vertstroke    <font color="#993399">0x10004b9</font>  <i><font color="#9A1900">/* U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_SHHA              <font color="#993399">0x10004ba</font>  <i><font color="#9A1900">/* U+04BA CYRILLIC CAPITAL LETTER SHHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_shha              <font color="#993399">0x10004bb</font>  <i><font color="#9A1900">/* U+04BB CYRILLIC SMALL LETTER SHHA */</font></i>

<b><font color="#000080">#define</font></b> XK_Cyrillic_SCHWA             <font color="#993399">0x10004d8</font>  <i><font color="#9A1900">/* U+04D8 CYRILLIC CAPITAL LETTER SCHWA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_schwa             <font color="#993399">0x10004d9</font>  <i><font color="#9A1900">/* U+04D9 CYRILLIC SMALL LETTER SCHWA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_I_macron          <font color="#993399">0x10004e2</font>  <i><font color="#9A1900">/* U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_i_macron          <font color="#993399">0x10004e3</font>  <i><font color="#9A1900">/* U+04E3 CYRILLIC SMALL LETTER I WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_O_bar             <font color="#993399">0x10004e8</font>  <i><font color="#9A1900">/* U+04E8 CYRILLIC CAPITAL LETTER BARRED O */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_o_bar             <font color="#993399">0x10004e9</font>  <i><font color="#9A1900">/* U+04E9 CYRILLIC SMALL LETTER BARRED O */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_U_macron          <font color="#993399">0x10004ee</font>  <i><font color="#9A1900">/* U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_u_macron          <font color="#993399">0x10004ef</font>  <i><font color="#9A1900">/* U+04EF CYRILLIC SMALL LETTER U WITH MACRON */</font></i>

<b><font color="#000080">#define</font></b> XK_Serbian_dje                   <font color="#993399">0x06a1</font>  <i><font color="#9A1900">/* U+0452 CYRILLIC SMALL LETTER DJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Macedonia_gje                 <font color="#993399">0x06a2</font>  <i><font color="#9A1900">/* U+0453 CYRILLIC SMALL LETTER GJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_io                   <font color="#993399">0x06a3</font>  <i><font color="#9A1900">/* U+0451 CYRILLIC SMALL LETTER IO */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukrainian_ie                  <font color="#993399">0x06a4</font>  <i><font color="#9A1900">/* U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukranian_je                   <font color="#993399">0x06a4</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Macedonia_dse                 <font color="#993399">0x06a5</font>  <i><font color="#9A1900">/* U+0455 CYRILLIC SMALL LETTER DZE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukrainian_i                   <font color="#993399">0x06a6</font>  <i><font color="#9A1900">/* U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukranian_i                    <font color="#993399">0x06a6</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukrainian_yi                  <font color="#993399">0x06a7</font>  <i><font color="#9A1900">/* U+0457 CYRILLIC SMALL LETTER YI */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukranian_yi                   <font color="#993399">0x06a7</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_je                   <font color="#993399">0x06a8</font>  <i><font color="#9A1900">/* U+0458 CYRILLIC SMALL LETTER JE */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_je                    <font color="#993399">0x06a8</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_lje                  <font color="#993399">0x06a9</font>  <i><font color="#9A1900">/* U+0459 CYRILLIC SMALL LETTER LJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_lje                   <font color="#993399">0x06a9</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_nje                  <font color="#993399">0x06aa</font>  <i><font color="#9A1900">/* U+045A CYRILLIC SMALL LETTER NJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_nje                   <font color="#993399">0x06aa</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_tshe                  <font color="#993399">0x06ab</font>  <i><font color="#9A1900">/* U+045B CYRILLIC SMALL LETTER TSHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Macedonia_kje                 <font color="#993399">0x06ac</font>  <i><font color="#9A1900">/* U+045C CYRILLIC SMALL LETTER KJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukrainian_ghe_with_upturn     <font color="#993399">0x06ad</font>  <i><font color="#9A1900">/* U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN */</font></i>
<b><font color="#000080">#define</font></b> XK_Byelorussian_shortu           <font color="#993399">0x06ae</font>  <i><font color="#9A1900">/* U+045E CYRILLIC SMALL LETTER SHORT U */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_dzhe                 <font color="#993399">0x06af</font>  <i><font color="#9A1900">/* U+045F CYRILLIC SMALL LETTER DZHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_dze                   <font color="#993399">0x06af</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_numerosign                    <font color="#993399">0x06b0</font>  <i><font color="#9A1900">/* U+2116 NUMERO SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_DJE                   <font color="#993399">0x06b1</font>  <i><font color="#9A1900">/* U+0402 CYRILLIC CAPITAL LETTER DJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Macedonia_GJE                 <font color="#993399">0x06b2</font>  <i><font color="#9A1900">/* U+0403 CYRILLIC CAPITAL LETTER GJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_IO                   <font color="#993399">0x06b3</font>  <i><font color="#9A1900">/* U+0401 CYRILLIC CAPITAL LETTER IO */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukrainian_IE                  <font color="#993399">0x06b4</font>  <i><font color="#9A1900">/* U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukranian_JE                   <font color="#993399">0x06b4</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Macedonia_DSE                 <font color="#993399">0x06b5</font>  <i><font color="#9A1900">/* U+0405 CYRILLIC CAPITAL LETTER DZE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukrainian_I                   <font color="#993399">0x06b6</font>  <i><font color="#9A1900">/* U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukranian_I                    <font color="#993399">0x06b6</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukrainian_YI                  <font color="#993399">0x06b7</font>  <i><font color="#9A1900">/* U+0407 CYRILLIC CAPITAL LETTER YI */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukranian_YI                   <font color="#993399">0x06b7</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_JE                   <font color="#993399">0x06b8</font>  <i><font color="#9A1900">/* U+0408 CYRILLIC CAPITAL LETTER JE */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_JE                    <font color="#993399">0x06b8</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_LJE                  <font color="#993399">0x06b9</font>  <i><font color="#9A1900">/* U+0409 CYRILLIC CAPITAL LETTER LJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_LJE                   <font color="#993399">0x06b9</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_NJE                  <font color="#993399">0x06ba</font>  <i><font color="#9A1900">/* U+040A CYRILLIC CAPITAL LETTER NJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_NJE                   <font color="#993399">0x06ba</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_TSHE                  <font color="#993399">0x06bb</font>  <i><font color="#9A1900">/* U+040B CYRILLIC CAPITAL LETTER TSHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Macedonia_KJE                 <font color="#993399">0x06bc</font>  <i><font color="#9A1900">/* U+040C CYRILLIC CAPITAL LETTER KJE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ukrainian_GHE_WITH_UPTURN     <font color="#993399">0x06bd</font>  <i><font color="#9A1900">/* U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN */</font></i>
<b><font color="#000080">#define</font></b> XK_Byelorussian_SHORTU           <font color="#993399">0x06be</font>  <i><font color="#9A1900">/* U+040E CYRILLIC CAPITAL LETTER SHORT U */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_DZHE                 <font color="#993399">0x06bf</font>  <i><font color="#9A1900">/* U+040F CYRILLIC CAPITAL LETTER DZHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Serbian_DZE                   <font color="#993399">0x06bf</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_yu                   <font color="#993399">0x06c0</font>  <i><font color="#9A1900">/* U+044E CYRILLIC SMALL LETTER YU */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_a                    <font color="#993399">0x06c1</font>  <i><font color="#9A1900">/* U+0430 CYRILLIC SMALL LETTER A */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_be                   <font color="#993399">0x06c2</font>  <i><font color="#9A1900">/* U+0431 CYRILLIC SMALL LETTER BE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_tse                  <font color="#993399">0x06c3</font>  <i><font color="#9A1900">/* U+0446 CYRILLIC SMALL LETTER TSE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_de                   <font color="#993399">0x06c4</font>  <i><font color="#9A1900">/* U+0434 CYRILLIC SMALL LETTER DE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ie                   <font color="#993399">0x06c5</font>  <i><font color="#9A1900">/* U+0435 CYRILLIC SMALL LETTER IE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ef                   <font color="#993399">0x06c6</font>  <i><font color="#9A1900">/* U+0444 CYRILLIC SMALL LETTER EF */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ghe                  <font color="#993399">0x06c7</font>  <i><font color="#9A1900">/* U+0433 CYRILLIC SMALL LETTER GHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ha                   <font color="#993399">0x06c8</font>  <i><font color="#9A1900">/* U+0445 CYRILLIC SMALL LETTER HA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_i                    <font color="#993399">0x06c9</font>  <i><font color="#9A1900">/* U+0438 CYRILLIC SMALL LETTER I */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_shorti               <font color="#993399">0x06ca</font>  <i><font color="#9A1900">/* U+0439 CYRILLIC SMALL LETTER SHORT I */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ka                   <font color="#993399">0x06cb</font>  <i><font color="#9A1900">/* U+043A CYRILLIC SMALL LETTER KA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_el                   <font color="#993399">0x06cc</font>  <i><font color="#9A1900">/* U+043B CYRILLIC SMALL LETTER EL */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_em                   <font color="#993399">0x06cd</font>  <i><font color="#9A1900">/* U+043C CYRILLIC SMALL LETTER EM */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_en                   <font color="#993399">0x06ce</font>  <i><font color="#9A1900">/* U+043D CYRILLIC SMALL LETTER EN */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_o                    <font color="#993399">0x06cf</font>  <i><font color="#9A1900">/* U+043E CYRILLIC SMALL LETTER O */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_pe                   <font color="#993399">0x06d0</font>  <i><font color="#9A1900">/* U+043F CYRILLIC SMALL LETTER PE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ya                   <font color="#993399">0x06d1</font>  <i><font color="#9A1900">/* U+044F CYRILLIC SMALL LETTER YA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_er                   <font color="#993399">0x06d2</font>  <i><font color="#9A1900">/* U+0440 CYRILLIC SMALL LETTER ER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_es                   <font color="#993399">0x06d3</font>  <i><font color="#9A1900">/* U+0441 CYRILLIC SMALL LETTER ES */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_te                   <font color="#993399">0x06d4</font>  <i><font color="#9A1900">/* U+0442 CYRILLIC SMALL LETTER TE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_u                    <font color="#993399">0x06d5</font>  <i><font color="#9A1900">/* U+0443 CYRILLIC SMALL LETTER U */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_zhe                  <font color="#993399">0x06d6</font>  <i><font color="#9A1900">/* U+0436 CYRILLIC SMALL LETTER ZHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ve                   <font color="#993399">0x06d7</font>  <i><font color="#9A1900">/* U+0432 CYRILLIC SMALL LETTER VE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_softsign             <font color="#993399">0x06d8</font>  <i><font color="#9A1900">/* U+044C CYRILLIC SMALL LETTER SOFT SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_yeru                 <font color="#993399">0x06d9</font>  <i><font color="#9A1900">/* U+044B CYRILLIC SMALL LETTER YERU */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ze                   <font color="#993399">0x06da</font>  <i><font color="#9A1900">/* U+0437 CYRILLIC SMALL LETTER ZE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_sha                  <font color="#993399">0x06db</font>  <i><font color="#9A1900">/* U+0448 CYRILLIC SMALL LETTER SHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_e                    <font color="#993399">0x06dc</font>  <i><font color="#9A1900">/* U+044D CYRILLIC SMALL LETTER E */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_shcha                <font color="#993399">0x06dd</font>  <i><font color="#9A1900">/* U+0449 CYRILLIC SMALL LETTER SHCHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_che                  <font color="#993399">0x06de</font>  <i><font color="#9A1900">/* U+0447 CYRILLIC SMALL LETTER CHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_hardsign             <font color="#993399">0x06df</font>  <i><font color="#9A1900">/* U+044A CYRILLIC SMALL LETTER HARD SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_YU                   <font color="#993399">0x06e0</font>  <i><font color="#9A1900">/* U+042E CYRILLIC CAPITAL LETTER YU */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_A                    <font color="#993399">0x06e1</font>  <i><font color="#9A1900">/* U+0410 CYRILLIC CAPITAL LETTER A */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_BE                   <font color="#993399">0x06e2</font>  <i><font color="#9A1900">/* U+0411 CYRILLIC CAPITAL LETTER BE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_TSE                  <font color="#993399">0x06e3</font>  <i><font color="#9A1900">/* U+0426 CYRILLIC CAPITAL LETTER TSE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_DE                   <font color="#993399">0x06e4</font>  <i><font color="#9A1900">/* U+0414 CYRILLIC CAPITAL LETTER DE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_IE                   <font color="#993399">0x06e5</font>  <i><font color="#9A1900">/* U+0415 CYRILLIC CAPITAL LETTER IE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_EF                   <font color="#993399">0x06e6</font>  <i><font color="#9A1900">/* U+0424 CYRILLIC CAPITAL LETTER EF */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_GHE                  <font color="#993399">0x06e7</font>  <i><font color="#9A1900">/* U+0413 CYRILLIC CAPITAL LETTER GHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_HA                   <font color="#993399">0x06e8</font>  <i><font color="#9A1900">/* U+0425 CYRILLIC CAPITAL LETTER HA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_I                    <font color="#993399">0x06e9</font>  <i><font color="#9A1900">/* U+0418 CYRILLIC CAPITAL LETTER I */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_SHORTI               <font color="#993399">0x06ea</font>  <i><font color="#9A1900">/* U+0419 CYRILLIC CAPITAL LETTER SHORT I */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_KA                   <font color="#993399">0x06eb</font>  <i><font color="#9A1900">/* U+041A CYRILLIC CAPITAL LETTER KA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_EL                   <font color="#993399">0x06ec</font>  <i><font color="#9A1900">/* U+041B CYRILLIC CAPITAL LETTER EL */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_EM                   <font color="#993399">0x06ed</font>  <i><font color="#9A1900">/* U+041C CYRILLIC CAPITAL LETTER EM */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_EN                   <font color="#993399">0x06ee</font>  <i><font color="#9A1900">/* U+041D CYRILLIC CAPITAL LETTER EN */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_O                    <font color="#993399">0x06ef</font>  <i><font color="#9A1900">/* U+041E CYRILLIC CAPITAL LETTER O */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_PE                   <font color="#993399">0x06f0</font>  <i><font color="#9A1900">/* U+041F CYRILLIC CAPITAL LETTER PE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_YA                   <font color="#993399">0x06f1</font>  <i><font color="#9A1900">/* U+042F CYRILLIC CAPITAL LETTER YA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ER                   <font color="#993399">0x06f2</font>  <i><font color="#9A1900">/* U+0420 CYRILLIC CAPITAL LETTER ER */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ES                   <font color="#993399">0x06f3</font>  <i><font color="#9A1900">/* U+0421 CYRILLIC CAPITAL LETTER ES */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_TE                   <font color="#993399">0x06f4</font>  <i><font color="#9A1900">/* U+0422 CYRILLIC CAPITAL LETTER TE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_U                    <font color="#993399">0x06f5</font>  <i><font color="#9A1900">/* U+0423 CYRILLIC CAPITAL LETTER U */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ZHE                  <font color="#993399">0x06f6</font>  <i><font color="#9A1900">/* U+0416 CYRILLIC CAPITAL LETTER ZHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_VE                   <font color="#993399">0x06f7</font>  <i><font color="#9A1900">/* U+0412 CYRILLIC CAPITAL LETTER VE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_SOFTSIGN             <font color="#993399">0x06f8</font>  <i><font color="#9A1900">/* U+042C CYRILLIC CAPITAL LETTER SOFT SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_YERU                 <font color="#993399">0x06f9</font>  <i><font color="#9A1900">/* U+042B CYRILLIC CAPITAL LETTER YERU */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_ZE                   <font color="#993399">0x06fa</font>  <i><font color="#9A1900">/* U+0417 CYRILLIC CAPITAL LETTER ZE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_SHA                  <font color="#993399">0x06fb</font>  <i><font color="#9A1900">/* U+0428 CYRILLIC CAPITAL LETTER SHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_E                    <font color="#993399">0x06fc</font>  <i><font color="#9A1900">/* U+042D CYRILLIC CAPITAL LETTER E */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_SHCHA                <font color="#993399">0x06fd</font>  <i><font color="#9A1900">/* U+0429 CYRILLIC CAPITAL LETTER SHCHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_CHE                  <font color="#993399">0x06fe</font>  <i><font color="#9A1900">/* U+0427 CYRILLIC CAPITAL LETTER CHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Cyrillic_HARDSIGN             <font color="#993399">0x06ff</font>  <i><font color="#9A1900">/* U+042A CYRILLIC CAPITAL LETTER HARD SIGN */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_CYRILLIC */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Greek</font></i>
<i><font color="#9A1900"> * (based on an early draft of, and not quite identical to, ISO/IEC 8859-7)</font></i>
<i><font color="#9A1900"> * Byte 3 = 7</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_GREEK
<b><font color="#000080">#define</font></b> XK_Greek_ALPHAaccent             <font color="#993399">0x07a1</font>  <i><font color="#9A1900">/* U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_EPSILONaccent           <font color="#993399">0x07a2</font>  <i><font color="#9A1900">/* U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_ETAaccent               <font color="#993399">0x07a3</font>  <i><font color="#9A1900">/* U+0389 GREEK CAPITAL LETTER ETA WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_IOTAaccent              <font color="#993399">0x07a4</font>  <i><font color="#9A1900">/* U+038A GREEK CAPITAL LETTER IOTA WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_IOTAdieresis            <font color="#993399">0x07a5</font>  <i><font color="#9A1900">/* U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_IOTAdiaeresis           <font color="#993399">0x07a5</font>  <i><font color="#9A1900">/* old typo */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_OMICRONaccent           <font color="#993399">0x07a7</font>  <i><font color="#9A1900">/* U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_UPSILONaccent           <font color="#993399">0x07a8</font>  <i><font color="#9A1900">/* U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_UPSILONdieresis         <font color="#993399">0x07a9</font>  <i><font color="#9A1900">/* U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_OMEGAaccent             <font color="#993399">0x07ab</font>  <i><font color="#9A1900">/* U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_accentdieresis          <font color="#993399">0x07ae</font>  <i><font color="#9A1900">/* U+0385 GREEK DIALYTIKA TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_horizbar                <font color="#993399">0x07af</font>  <i><font color="#9A1900">/* U+2015 HORIZONTAL BAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_alphaaccent             <font color="#993399">0x07b1</font>  <i><font color="#9A1900">/* U+03AC GREEK SMALL LETTER ALPHA WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_epsilonaccent           <font color="#993399">0x07b2</font>  <i><font color="#9A1900">/* U+03AD GREEK SMALL LETTER EPSILON WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_etaaccent               <font color="#993399">0x07b3</font>  <i><font color="#9A1900">/* U+03AE GREEK SMALL LETTER ETA WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_iotaaccent              <font color="#993399">0x07b4</font>  <i><font color="#9A1900">/* U+03AF GREEK SMALL LETTER IOTA WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_iotadieresis            <font color="#993399">0x07b5</font>  <i><font color="#9A1900">/* U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_iotaaccentdieresis      <font color="#993399">0x07b6</font>  <i><font color="#9A1900">/* U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_omicronaccent           <font color="#993399">0x07b7</font>  <i><font color="#9A1900">/* U+03CC GREEK SMALL LETTER OMICRON WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_upsilonaccent           <font color="#993399">0x07b8</font>  <i><font color="#9A1900">/* U+03CD GREEK SMALL LETTER UPSILON WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_upsilondieresis         <font color="#993399">0x07b9</font>  <i><font color="#9A1900">/* U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_upsilonaccentdieresis   <font color="#993399">0x07ba</font>  <i><font color="#9A1900">/* U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_omegaaccent             <font color="#993399">0x07bb</font>  <i><font color="#9A1900">/* U+03CE GREEK SMALL LETTER OMEGA WITH TONOS */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_ALPHA                   <font color="#993399">0x07c1</font>  <i><font color="#9A1900">/* U+0391 GREEK CAPITAL LETTER ALPHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_BETA                    <font color="#993399">0x07c2</font>  <i><font color="#9A1900">/* U+0392 GREEK CAPITAL LETTER BETA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_GAMMA                   <font color="#993399">0x07c3</font>  <i><font color="#9A1900">/* U+0393 GREEK CAPITAL LETTER GAMMA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_DELTA                   <font color="#993399">0x07c4</font>  <i><font color="#9A1900">/* U+0394 GREEK CAPITAL LETTER DELTA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_EPSILON                 <font color="#993399">0x07c5</font>  <i><font color="#9A1900">/* U+0395 GREEK CAPITAL LETTER EPSILON */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_ZETA                    <font color="#993399">0x07c6</font>  <i><font color="#9A1900">/* U+0396 GREEK CAPITAL LETTER ZETA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_ETA                     <font color="#993399">0x07c7</font>  <i><font color="#9A1900">/* U+0397 GREEK CAPITAL LETTER ETA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_THETA                   <font color="#993399">0x07c8</font>  <i><font color="#9A1900">/* U+0398 GREEK CAPITAL LETTER THETA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_IOTA                    <font color="#993399">0x07c9</font>  <i><font color="#9A1900">/* U+0399 GREEK CAPITAL LETTER IOTA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_KAPPA                   <font color="#993399">0x07ca</font>  <i><font color="#9A1900">/* U+039A GREEK CAPITAL LETTER KAPPA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_LAMDA                   <font color="#993399">0x07cb</font>  <i><font color="#9A1900">/* U+039B GREEK CAPITAL LETTER LAMDA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_LAMBDA                  <font color="#993399">0x07cb</font>  <i><font color="#9A1900">/* U+039B GREEK CAPITAL LETTER LAMDA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_MU                      <font color="#993399">0x07cc</font>  <i><font color="#9A1900">/* U+039C GREEK CAPITAL LETTER MU */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_NU                      <font color="#993399">0x07cd</font>  <i><font color="#9A1900">/* U+039D GREEK CAPITAL LETTER NU */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_XI                      <font color="#993399">0x07ce</font>  <i><font color="#9A1900">/* U+039E GREEK CAPITAL LETTER XI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_OMICRON                 <font color="#993399">0x07cf</font>  <i><font color="#9A1900">/* U+039F GREEK CAPITAL LETTER OMICRON */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_PI                      <font color="#993399">0x07d0</font>  <i><font color="#9A1900">/* U+03A0 GREEK CAPITAL LETTER PI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_RHO                     <font color="#993399">0x07d1</font>  <i><font color="#9A1900">/* U+03A1 GREEK CAPITAL LETTER RHO */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_SIGMA                   <font color="#993399">0x07d2</font>  <i><font color="#9A1900">/* U+03A3 GREEK CAPITAL LETTER SIGMA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_TAU                     <font color="#993399">0x07d4</font>  <i><font color="#9A1900">/* U+03A4 GREEK CAPITAL LETTER TAU */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_UPSILON                 <font color="#993399">0x07d5</font>  <i><font color="#9A1900">/* U+03A5 GREEK CAPITAL LETTER UPSILON */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_PHI                     <font color="#993399">0x07d6</font>  <i><font color="#9A1900">/* U+03A6 GREEK CAPITAL LETTER PHI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_CHI                     <font color="#993399">0x07d7</font>  <i><font color="#9A1900">/* U+03A7 GREEK CAPITAL LETTER CHI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_PSI                     <font color="#993399">0x07d8</font>  <i><font color="#9A1900">/* U+03A8 GREEK CAPITAL LETTER PSI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_OMEGA                   <font color="#993399">0x07d9</font>  <i><font color="#9A1900">/* U+03A9 GREEK CAPITAL LETTER OMEGA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_alpha                   <font color="#993399">0x07e1</font>  <i><font color="#9A1900">/* U+03B1 GREEK SMALL LETTER ALPHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_beta                    <font color="#993399">0x07e2</font>  <i><font color="#9A1900">/* U+03B2 GREEK SMALL LETTER BETA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_gamma                   <font color="#993399">0x07e3</font>  <i><font color="#9A1900">/* U+03B3 GREEK SMALL LETTER GAMMA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_delta                   <font color="#993399">0x07e4</font>  <i><font color="#9A1900">/* U+03B4 GREEK SMALL LETTER DELTA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_epsilon                 <font color="#993399">0x07e5</font>  <i><font color="#9A1900">/* U+03B5 GREEK SMALL LETTER EPSILON */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_zeta                    <font color="#993399">0x07e6</font>  <i><font color="#9A1900">/* U+03B6 GREEK SMALL LETTER ZETA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_eta                     <font color="#993399">0x07e7</font>  <i><font color="#9A1900">/* U+03B7 GREEK SMALL LETTER ETA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_theta                   <font color="#993399">0x07e8</font>  <i><font color="#9A1900">/* U+03B8 GREEK SMALL LETTER THETA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_iota                    <font color="#993399">0x07e9</font>  <i><font color="#9A1900">/* U+03B9 GREEK SMALL LETTER IOTA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_kappa                   <font color="#993399">0x07ea</font>  <i><font color="#9A1900">/* U+03BA GREEK SMALL LETTER KAPPA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_lamda                   <font color="#993399">0x07eb</font>  <i><font color="#9A1900">/* U+03BB GREEK SMALL LETTER LAMDA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_lambda                  <font color="#993399">0x07eb</font>  <i><font color="#9A1900">/* U+03BB GREEK SMALL LETTER LAMDA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_mu                      <font color="#993399">0x07ec</font>  <i><font color="#9A1900">/* U+03BC GREEK SMALL LETTER MU */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_nu                      <font color="#993399">0x07ed</font>  <i><font color="#9A1900">/* U+03BD GREEK SMALL LETTER NU */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_xi                      <font color="#993399">0x07ee</font>  <i><font color="#9A1900">/* U+03BE GREEK SMALL LETTER XI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_omicron                 <font color="#993399">0x07ef</font>  <i><font color="#9A1900">/* U+03BF GREEK SMALL LETTER OMICRON */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_pi                      <font color="#993399">0x07f0</font>  <i><font color="#9A1900">/* U+03C0 GREEK SMALL LETTER PI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_rho                     <font color="#993399">0x07f1</font>  <i><font color="#9A1900">/* U+03C1 GREEK SMALL LETTER RHO */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_sigma                   <font color="#993399">0x07f2</font>  <i><font color="#9A1900">/* U+03C3 GREEK SMALL LETTER SIGMA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_finalsmallsigma         <font color="#993399">0x07f3</font>  <i><font color="#9A1900">/* U+03C2 GREEK SMALL LETTER FINAL SIGMA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_tau                     <font color="#993399">0x07f4</font>  <i><font color="#9A1900">/* U+03C4 GREEK SMALL LETTER TAU */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_upsilon                 <font color="#993399">0x07f5</font>  <i><font color="#9A1900">/* U+03C5 GREEK SMALL LETTER UPSILON */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_phi                     <font color="#993399">0x07f6</font>  <i><font color="#9A1900">/* U+03C6 GREEK SMALL LETTER PHI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_chi                     <font color="#993399">0x07f7</font>  <i><font color="#9A1900">/* U+03C7 GREEK SMALL LETTER CHI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_psi                     <font color="#993399">0x07f8</font>  <i><font color="#9A1900">/* U+03C8 GREEK SMALL LETTER PSI */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_omega                   <font color="#993399">0x07f9</font>  <i><font color="#9A1900">/* U+03C9 GREEK SMALL LETTER OMEGA */</font></i>
<b><font color="#000080">#define</font></b> XK_Greek_switch                  <font color="#993399">0xff7e</font>  <i><font color="#9A1900">/* Alias for mode_switch */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_GREEK */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Technical</font></i>
<i><font color="#9A1900"> * (from the DEC VT330/VT420 Technical Character Set, </font></i><u><font color="#0000FF">http://vt100.net/charsets/technical.html</font></u><i><font color="#9A1900">)</font></i>
<i><font color="#9A1900"> * Byte 3 = 8</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_TECHNICAL
<b><font color="#000080">#define</font></b> XK_leftradical                   <font color="#993399">0x08a1</font>  <i><font color="#9A1900">/* U+23B7 RADICAL SYMBOL BOTTOM */</font></i>
<b><font color="#000080">#define</font></b> XK_topleftradical                <font color="#993399">0x08a2</font>  <i><font color="#9A1900">/*(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)*/</font></i>
<b><font color="#000080">#define</font></b> XK_horizconnector                <font color="#993399">0x08a3</font>  <i><font color="#9A1900">/*(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)*/</font></i>
<b><font color="#000080">#define</font></b> XK_topintegral                   <font color="#993399">0x08a4</font>  <i><font color="#9A1900">/* U+2320 TOP HALF INTEGRAL */</font></i>
<b><font color="#000080">#define</font></b> XK_botintegral                   <font color="#993399">0x08a5</font>  <i><font color="#9A1900">/* U+2321 BOTTOM HALF INTEGRAL */</font></i>
<b><font color="#000080">#define</font></b> XK_vertconnector                 <font color="#993399">0x08a6</font>  <i><font color="#9A1900">/*(U+2502 BOX DRAWINGS LIGHT VERTICAL)*/</font></i>
<b><font color="#000080">#define</font></b> XK_topleftsqbracket              <font color="#993399">0x08a7</font>  <i><font color="#9A1900">/* U+23A1 LEFT SQUARE BRACKET UPPER CORNER */</font></i>
<b><font color="#000080">#define</font></b> XK_botleftsqbracket              <font color="#993399">0x08a8</font>  <i><font color="#9A1900">/* U+23A3 LEFT SQUARE BRACKET LOWER CORNER */</font></i>
<b><font color="#000080">#define</font></b> XK_toprightsqbracket             <font color="#993399">0x08a9</font>  <i><font color="#9A1900">/* U+23A4 RIGHT SQUARE BRACKET UPPER CORNER */</font></i>
<b><font color="#000080">#define</font></b> XK_botrightsqbracket             <font color="#993399">0x08aa</font>  <i><font color="#9A1900">/* U+23A6 RIGHT SQUARE BRACKET LOWER CORNER */</font></i>
<b><font color="#000080">#define</font></b> XK_topleftparens                 <font color="#993399">0x08ab</font>  <i><font color="#9A1900">/* U+239B LEFT PARENTHESIS UPPER HOOK */</font></i>
<b><font color="#000080">#define</font></b> XK_botleftparens                 <font color="#993399">0x08ac</font>  <i><font color="#9A1900">/* U+239D LEFT PARENTHESIS LOWER HOOK */</font></i>
<b><font color="#000080">#define</font></b> XK_toprightparens                <font color="#993399">0x08ad</font>  <i><font color="#9A1900">/* U+239E RIGHT PARENTHESIS UPPER HOOK */</font></i>
<b><font color="#000080">#define</font></b> XK_botrightparens                <font color="#993399">0x08ae</font>  <i><font color="#9A1900">/* U+23A0 RIGHT PARENTHESIS LOWER HOOK */</font></i>
<b><font color="#000080">#define</font></b> XK_leftmiddlecurlybrace          <font color="#993399">0x08af</font>  <i><font color="#9A1900">/* U+23A8 LEFT CURLY BRACKET MIDDLE PIECE */</font></i>
<b><font color="#000080">#define</font></b> XK_rightmiddlecurlybrace         <font color="#993399">0x08b0</font>  <i><font color="#9A1900">/* U+23AC RIGHT CURLY BRACKET MIDDLE PIECE */</font></i>
<b><font color="#000080">#define</font></b> XK_topleftsummation              <font color="#993399">0x08b1</font>
<b><font color="#000080">#define</font></b> XK_botleftsummation              <font color="#993399">0x08b2</font>
<b><font color="#000080">#define</font></b> XK_topvertsummationconnector     <font color="#993399">0x08b3</font>
<b><font color="#000080">#define</font></b> XK_botvertsummationconnector     <font color="#993399">0x08b4</font>
<b><font color="#000080">#define</font></b> XK_toprightsummation             <font color="#993399">0x08b5</font>
<b><font color="#000080">#define</font></b> XK_botrightsummation             <font color="#993399">0x08b6</font>
<b><font color="#000080">#define</font></b> XK_rightmiddlesummation          <font color="#993399">0x08b7</font>
<b><font color="#000080">#define</font></b> XK_lessthanequal                 <font color="#993399">0x08bc</font>  <i><font color="#9A1900">/* U+2264 LESS-THAN OR EQUAL TO */</font></i>
<b><font color="#000080">#define</font></b> XK_notequal                      <font color="#993399">0x08bd</font>  <i><font color="#9A1900">/* U+2260 NOT EQUAL TO */</font></i>
<b><font color="#000080">#define</font></b> XK_greaterthanequal              <font color="#993399">0x08be</font>  <i><font color="#9A1900">/* U+2265 GREATER-THAN OR EQUAL TO */</font></i>
<b><font color="#000080">#define</font></b> XK_integral                      <font color="#993399">0x08bf</font>  <i><font color="#9A1900">/* U+222B INTEGRAL */</font></i>
<b><font color="#000080">#define</font></b> XK_therefore                     <font color="#993399">0x08c0</font>  <i><font color="#9A1900">/* U+2234 THEREFORE */</font></i>
<b><font color="#000080">#define</font></b> XK_variation                     <font color="#993399">0x08c1</font>  <i><font color="#9A1900">/* U+221D PROPORTIONAL TO */</font></i>
<b><font color="#000080">#define</font></b> XK_infinity                      <font color="#993399">0x08c2</font>  <i><font color="#9A1900">/* U+221E INFINITY */</font></i>
<b><font color="#000080">#define</font></b> XK_nabla                         <font color="#993399">0x08c5</font>  <i><font color="#9A1900">/* U+2207 NABLA */</font></i>
<b><font color="#000080">#define</font></b> XK_approximate                   <font color="#993399">0x08c8</font>  <i><font color="#9A1900">/* U+223C TILDE OPERATOR */</font></i>
<b><font color="#000080">#define</font></b> XK_similarequal                  <font color="#993399">0x08c9</font>  <i><font color="#9A1900">/* U+2243 ASYMPTOTICALLY EQUAL TO */</font></i>
<b><font color="#000080">#define</font></b> XK_ifonlyif                      <font color="#993399">0x08cd</font>  <i><font color="#9A1900">/* U+21D4 LEFT RIGHT DOUBLE ARROW */</font></i>
<b><font color="#000080">#define</font></b> XK_implies                       <font color="#993399">0x08ce</font>  <i><font color="#9A1900">/* U+21D2 RIGHTWARDS DOUBLE ARROW */</font></i>
<b><font color="#000080">#define</font></b> XK_identical                     <font color="#993399">0x08cf</font>  <i><font color="#9A1900">/* U+2261 IDENTICAL TO */</font></i>
<b><font color="#000080">#define</font></b> XK_radical                       <font color="#993399">0x08d6</font>  <i><font color="#9A1900">/* U+221A SQUARE ROOT */</font></i>
<b><font color="#000080">#define</font></b> XK_includedin                    <font color="#993399">0x08da</font>  <i><font color="#9A1900">/* U+2282 SUBSET OF */</font></i>
<b><font color="#000080">#define</font></b> XK_includes                      <font color="#993399">0x08db</font>  <i><font color="#9A1900">/* U+2283 SUPERSET OF */</font></i>
<b><font color="#000080">#define</font></b> XK_intersection                  <font color="#993399">0x08dc</font>  <i><font color="#9A1900">/* U+2229 INTERSECTION */</font></i>
<b><font color="#000080">#define</font></b> XK_union                         <font color="#993399">0x08dd</font>  <i><font color="#9A1900">/* U+222A UNION */</font></i>
<b><font color="#000080">#define</font></b> XK_logicaland                    <font color="#993399">0x08de</font>  <i><font color="#9A1900">/* U+2227 LOGICAL AND */</font></i>
<b><font color="#000080">#define</font></b> XK_logicalor                     <font color="#993399">0x08df</font>  <i><font color="#9A1900">/* U+2228 LOGICAL OR */</font></i>
<b><font color="#000080">#define</font></b> XK_partialderivative             <font color="#993399">0x08ef</font>  <i><font color="#9A1900">/* U+2202 PARTIAL DIFFERENTIAL */</font></i>
<b><font color="#000080">#define</font></b> XK_function                      <font color="#993399">0x08f6</font>  <i><font color="#9A1900">/* U+0192 LATIN SMALL LETTER F WITH HOOK */</font></i>
<b><font color="#000080">#define</font></b> XK_leftarrow                     <font color="#993399">0x08fb</font>  <i><font color="#9A1900">/* U+2190 LEFTWARDS ARROW */</font></i>
<b><font color="#000080">#define</font></b> XK_uparrow                       <font color="#993399">0x08fc</font>  <i><font color="#9A1900">/* U+2191 UPWARDS ARROW */</font></i>
<b><font color="#000080">#define</font></b> XK_rightarrow                    <font color="#993399">0x08fd</font>  <i><font color="#9A1900">/* U+2192 RIGHTWARDS ARROW */</font></i>
<b><font color="#000080">#define</font></b> XK_downarrow                     <font color="#993399">0x08fe</font>  <i><font color="#9A1900">/* U+2193 DOWNWARDS ARROW */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_TECHNICAL */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Special</font></i>
<i><font color="#9A1900"> * (from the DEC VT100 Special Graphics Character Set)</font></i>
<i><font color="#9A1900"> * Byte 3 = 9</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_SPECIAL
<b><font color="#000080">#define</font></b> XK_blank                         <font color="#993399">0x09df</font>
<b><font color="#000080">#define</font></b> XK_soliddiamond                  <font color="#993399">0x09e0</font>  <i><font color="#9A1900">/* U+25C6 BLACK DIAMOND */</font></i>
<b><font color="#000080">#define</font></b> XK_checkerboard                  <font color="#993399">0x09e1</font>  <i><font color="#9A1900">/* U+2592 MEDIUM SHADE */</font></i>
<b><font color="#000080">#define</font></b> XK_ht                            <font color="#993399">0x09e2</font>  <i><font color="#9A1900">/* U+2409 SYMBOL FOR HORIZONTAL TABULATION */</font></i>
<b><font color="#000080">#define</font></b> XK_ff                            <font color="#993399">0x09e3</font>  <i><font color="#9A1900">/* U+240C SYMBOL FOR FORM FEED */</font></i>
<b><font color="#000080">#define</font></b> XK_cr                            <font color="#993399">0x09e4</font>  <i><font color="#9A1900">/* U+240D SYMBOL FOR CARRIAGE RETURN */</font></i>
<b><font color="#000080">#define</font></b> XK_lf                            <font color="#993399">0x09e5</font>  <i><font color="#9A1900">/* U+240A SYMBOL FOR LINE FEED */</font></i>
<b><font color="#000080">#define</font></b> XK_nl                            <font color="#993399">0x09e8</font>  <i><font color="#9A1900">/* U+2424 SYMBOL FOR NEWLINE */</font></i>
<b><font color="#000080">#define</font></b> XK_vt                            <font color="#993399">0x09e9</font>  <i><font color="#9A1900">/* U+240B SYMBOL FOR VERTICAL TABULATION */</font></i>
<b><font color="#000080">#define</font></b> XK_lowrightcorner                <font color="#993399">0x09ea</font>  <i><font color="#9A1900">/* U+2518 BOX DRAWINGS LIGHT UP AND LEFT */</font></i>
<b><font color="#000080">#define</font></b> XK_uprightcorner                 <font color="#993399">0x09eb</font>  <i><font color="#9A1900">/* U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT */</font></i>
<b><font color="#000080">#define</font></b> XK_upleftcorner                  <font color="#993399">0x09ec</font>  <i><font color="#9A1900">/* U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_lowleftcorner                 <font color="#993399">0x09ed</font>  <i><font color="#9A1900">/* U+2514 BOX DRAWINGS LIGHT UP AND RIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_crossinglines                 <font color="#993399">0x09ee</font>  <i><font color="#9A1900">/* U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL */</font></i>
<b><font color="#000080">#define</font></b> XK_horizlinescan1                <font color="#993399">0x09ef</font>  <i><font color="#9A1900">/* U+23BA HORIZONTAL SCAN LINE-1 */</font></i>
<b><font color="#000080">#define</font></b> XK_horizlinescan3                <font color="#993399">0x09f0</font>  <i><font color="#9A1900">/* U+23BB HORIZONTAL SCAN LINE-3 */</font></i>
<b><font color="#000080">#define</font></b> XK_horizlinescan5                <font color="#993399">0x09f1</font>  <i><font color="#9A1900">/* U+2500 BOX DRAWINGS LIGHT HORIZONTAL */</font></i>
<b><font color="#000080">#define</font></b> XK_horizlinescan7                <font color="#993399">0x09f2</font>  <i><font color="#9A1900">/* U+23BC HORIZONTAL SCAN LINE-7 */</font></i>
<b><font color="#000080">#define</font></b> XK_horizlinescan9                <font color="#993399">0x09f3</font>  <i><font color="#9A1900">/* U+23BD HORIZONTAL SCAN LINE-9 */</font></i>
<b><font color="#000080">#define</font></b> XK_leftt                         <font color="#993399">0x09f4</font>  <i><font color="#9A1900">/* U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_rightt                        <font color="#993399">0x09f5</font>  <i><font color="#9A1900">/* U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT */</font></i>
<b><font color="#000080">#define</font></b> XK_bott                          <font color="#993399">0x09f6</font>  <i><font color="#9A1900">/* U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL */</font></i>
<b><font color="#000080">#define</font></b> XK_topt                          <font color="#993399">0x09f7</font>  <i><font color="#9A1900">/* U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL */</font></i>
<b><font color="#000080">#define</font></b> XK_vertbar                       <font color="#993399">0x09f8</font>  <i><font color="#9A1900">/* U+2502 BOX DRAWINGS LIGHT VERTICAL */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_SPECIAL */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Publishing</font></i>
<i><font color="#9A1900"> * (these are probably from a long forgotten DEC Publishing</font></i>
<i><font color="#9A1900"> * font that once shipped with DECwrite)</font></i>
<i><font color="#9A1900"> * Byte 3 = 0x0a</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_PUBLISHING
<b><font color="#000080">#define</font></b> XK_emspace                       <font color="#993399">0x0aa1</font>  <i><font color="#9A1900">/* U+2003 EM SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_enspace                       <font color="#993399">0x0aa2</font>  <i><font color="#9A1900">/* U+2002 EN SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_em3space                      <font color="#993399">0x0aa3</font>  <i><font color="#9A1900">/* U+2004 THREE-PER-EM SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_em4space                      <font color="#993399">0x0aa4</font>  <i><font color="#9A1900">/* U+2005 FOUR-PER-EM SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_digitspace                    <font color="#993399">0x0aa5</font>  <i><font color="#9A1900">/* U+2007 FIGURE SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_punctspace                    <font color="#993399">0x0aa6</font>  <i><font color="#9A1900">/* U+2008 PUNCTUATION SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_thinspace                     <font color="#993399">0x0aa7</font>  <i><font color="#9A1900">/* U+2009 THIN SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_hairspace                     <font color="#993399">0x0aa8</font>  <i><font color="#9A1900">/* U+200A HAIR SPACE */</font></i>
<b><font color="#000080">#define</font></b> XK_emdash                        <font color="#993399">0x0aa9</font>  <i><font color="#9A1900">/* U+2014 EM DASH */</font></i>
<b><font color="#000080">#define</font></b> XK_endash                        <font color="#993399">0x0aaa</font>  <i><font color="#9A1900">/* U+2013 EN DASH */</font></i>
<b><font color="#000080">#define</font></b> XK_signifblank                   <font color="#993399">0x0aac</font>  <i><font color="#9A1900">/*(U+2423 OPEN BOX)*/</font></i>
<b><font color="#000080">#define</font></b> XK_ellipsis                      <font color="#993399">0x0aae</font>  <i><font color="#9A1900">/* U+2026 HORIZONTAL ELLIPSIS */</font></i>
<b><font color="#000080">#define</font></b> XK_doubbaselinedot               <font color="#993399">0x0aaf</font>  <i><font color="#9A1900">/* U+2025 TWO DOT LEADER */</font></i>
<b><font color="#000080">#define</font></b> XK_onethird                      <font color="#993399">0x0ab0</font>  <i><font color="#9A1900">/* U+2153 VULGAR FRACTION ONE THIRD */</font></i>
<b><font color="#000080">#define</font></b> XK_twothirds                     <font color="#993399">0x0ab1</font>  <i><font color="#9A1900">/* U+2154 VULGAR FRACTION TWO THIRDS */</font></i>
<b><font color="#000080">#define</font></b> XK_onefifth                      <font color="#993399">0x0ab2</font>  <i><font color="#9A1900">/* U+2155 VULGAR FRACTION ONE FIFTH */</font></i>
<b><font color="#000080">#define</font></b> XK_twofifths                     <font color="#993399">0x0ab3</font>  <i><font color="#9A1900">/* U+2156 VULGAR FRACTION TWO FIFTHS */</font></i>
<b><font color="#000080">#define</font></b> XK_threefifths                   <font color="#993399">0x0ab4</font>  <i><font color="#9A1900">/* U+2157 VULGAR FRACTION THREE FIFTHS */</font></i>
<b><font color="#000080">#define</font></b> XK_fourfifths                    <font color="#993399">0x0ab5</font>  <i><font color="#9A1900">/* U+2158 VULGAR FRACTION FOUR FIFTHS */</font></i>
<b><font color="#000080">#define</font></b> XK_onesixth                      <font color="#993399">0x0ab6</font>  <i><font color="#9A1900">/* U+2159 VULGAR FRACTION ONE SIXTH */</font></i>
<b><font color="#000080">#define</font></b> XK_fivesixths                    <font color="#993399">0x0ab7</font>  <i><font color="#9A1900">/* U+215A VULGAR FRACTION FIVE SIXTHS */</font></i>
<b><font color="#000080">#define</font></b> XK_careof                        <font color="#993399">0x0ab8</font>  <i><font color="#9A1900">/* U+2105 CARE OF */</font></i>
<b><font color="#000080">#define</font></b> XK_figdash                       <font color="#993399">0x0abb</font>  <i><font color="#9A1900">/* U+2012 FIGURE DASH */</font></i>
<b><font color="#000080">#define</font></b> XK_leftanglebracket              <font color="#993399">0x0abc</font>  <i><font color="#9A1900">/*(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)*/</font></i>
<b><font color="#000080">#define</font></b> XK_decimalpoint                  <font color="#993399">0x0abd</font>  <i><font color="#9A1900">/*(U+002E FULL STOP)*/</font></i>
<b><font color="#000080">#define</font></b> XK_rightanglebracket             <font color="#993399">0x0abe</font>  <i><font color="#9A1900">/*(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)*/</font></i>
<b><font color="#000080">#define</font></b> XK_marker                        <font color="#993399">0x0abf</font>
<b><font color="#000080">#define</font></b> XK_oneeighth                     <font color="#993399">0x0ac3</font>  <i><font color="#9A1900">/* U+215B VULGAR FRACTION ONE EIGHTH */</font></i>
<b><font color="#000080">#define</font></b> XK_threeeighths                  <font color="#993399">0x0ac4</font>  <i><font color="#9A1900">/* U+215C VULGAR FRACTION THREE EIGHTHS */</font></i>
<b><font color="#000080">#define</font></b> XK_fiveeighths                   <font color="#993399">0x0ac5</font>  <i><font color="#9A1900">/* U+215D VULGAR FRACTION FIVE EIGHTHS */</font></i>
<b><font color="#000080">#define</font></b> XK_seveneighths                  <font color="#993399">0x0ac6</font>  <i><font color="#9A1900">/* U+215E VULGAR FRACTION SEVEN EIGHTHS */</font></i>
<b><font color="#000080">#define</font></b> XK_trademark                     <font color="#993399">0x0ac9</font>  <i><font color="#9A1900">/* U+2122 TRADE MARK SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_signaturemark                 <font color="#993399">0x0aca</font>  <i><font color="#9A1900">/*(U+2613 SALTIRE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_trademarkincircle             <font color="#993399">0x0acb</font>
<b><font color="#000080">#define</font></b> XK_leftopentriangle              <font color="#993399">0x0acc</font>  <i><font color="#9A1900">/*(U+25C1 WHITE LEFT-POINTING TRIANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_rightopentriangle             <font color="#993399">0x0acd</font>  <i><font color="#9A1900">/*(U+25B7 WHITE RIGHT-POINTING TRIANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_emopencircle                  <font color="#993399">0x0ace</font>  <i><font color="#9A1900">/*(U+25CB WHITE CIRCLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_emopenrectangle               <font color="#993399">0x0acf</font>  <i><font color="#9A1900">/*(U+25AF WHITE VERTICAL RECTANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_leftsinglequotemark           <font color="#993399">0x0ad0</font>  <i><font color="#9A1900">/* U+2018 LEFT SINGLE QUOTATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_rightsinglequotemark          <font color="#993399">0x0ad1</font>  <i><font color="#9A1900">/* U+2019 RIGHT SINGLE QUOTATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_leftdoublequotemark           <font color="#993399">0x0ad2</font>  <i><font color="#9A1900">/* U+201C LEFT DOUBLE QUOTATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_rightdoublequotemark          <font color="#993399">0x0ad3</font>  <i><font color="#9A1900">/* U+201D RIGHT DOUBLE QUOTATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_prescription                  <font color="#993399">0x0ad4</font>  <i><font color="#9A1900">/* U+211E PRESCRIPTION TAKE */</font></i>
<b><font color="#000080">#define</font></b> XK_minutes                       <font color="#993399">0x0ad6</font>  <i><font color="#9A1900">/* U+2032 PRIME */</font></i>
<b><font color="#000080">#define</font></b> XK_seconds                       <font color="#993399">0x0ad7</font>  <i><font color="#9A1900">/* U+2033 DOUBLE PRIME */</font></i>
<b><font color="#000080">#define</font></b> XK_latincross                    <font color="#993399">0x0ad9</font>  <i><font color="#9A1900">/* U+271D LATIN CROSS */</font></i>
<b><font color="#000080">#define</font></b> XK_hexagram                      <font color="#993399">0x0ada</font>
<b><font color="#000080">#define</font></b> XK_filledrectbullet              <font color="#993399">0x0adb</font>  <i><font color="#9A1900">/*(U+25AC BLACK RECTANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_filledlefttribullet           <font color="#993399">0x0adc</font>  <i><font color="#9A1900">/*(U+25C0 BLACK LEFT-POINTING TRIANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_filledrighttribullet          <font color="#993399">0x0add</font>  <i><font color="#9A1900">/*(U+25B6 BLACK RIGHT-POINTING TRIANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_emfilledcircle                <font color="#993399">0x0ade</font>  <i><font color="#9A1900">/*(U+25CF BLACK CIRCLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_emfilledrect                  <font color="#993399">0x0adf</font>  <i><font color="#9A1900">/*(U+25AE BLACK VERTICAL RECTANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_enopencircbullet              <font color="#993399">0x0ae0</font>  <i><font color="#9A1900">/*(U+25E6 WHITE BULLET)*/</font></i>
<b><font color="#000080">#define</font></b> XK_enopensquarebullet            <font color="#993399">0x0ae1</font>  <i><font color="#9A1900">/*(U+25AB WHITE SMALL SQUARE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_openrectbullet                <font color="#993399">0x0ae2</font>  <i><font color="#9A1900">/*(U+25AD WHITE RECTANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_opentribulletup               <font color="#993399">0x0ae3</font>  <i><font color="#9A1900">/*(U+25B3 WHITE UP-POINTING TRIANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_opentribulletdown             <font color="#993399">0x0ae4</font>  <i><font color="#9A1900">/*(U+25BD WHITE DOWN-POINTING TRIANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_openstar                      <font color="#993399">0x0ae5</font>  <i><font color="#9A1900">/*(U+2606 WHITE STAR)*/</font></i>
<b><font color="#000080">#define</font></b> XK_enfilledcircbullet            <font color="#993399">0x0ae6</font>  <i><font color="#9A1900">/*(U+2022 BULLET)*/</font></i>
<b><font color="#000080">#define</font></b> XK_enfilledsqbullet              <font color="#993399">0x0ae7</font>  <i><font color="#9A1900">/*(U+25AA BLACK SMALL SQUARE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_filledtribulletup             <font color="#993399">0x0ae8</font>  <i><font color="#9A1900">/*(U+25B2 BLACK UP-POINTING TRIANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_filledtribulletdown           <font color="#993399">0x0ae9</font>  <i><font color="#9A1900">/*(U+25BC BLACK DOWN-POINTING TRIANGLE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_leftpointer                   <font color="#993399">0x0aea</font>  <i><font color="#9A1900">/*(U+261C WHITE LEFT POINTING INDEX)*/</font></i>
<b><font color="#000080">#define</font></b> XK_rightpointer                  <font color="#993399">0x0aeb</font>  <i><font color="#9A1900">/*(U+261E WHITE RIGHT POINTING INDEX)*/</font></i>
<b><font color="#000080">#define</font></b> XK_club                          <font color="#993399">0x0aec</font>  <i><font color="#9A1900">/* U+2663 BLACK CLUB SUIT */</font></i>
<b><font color="#000080">#define</font></b> XK_diamond                       <font color="#993399">0x0aed</font>  <i><font color="#9A1900">/* U+2666 BLACK DIAMOND SUIT */</font></i>
<b><font color="#000080">#define</font></b> XK_heart                         <font color="#993399">0x0aee</font>  <i><font color="#9A1900">/* U+2665 BLACK HEART SUIT */</font></i>
<b><font color="#000080">#define</font></b> XK_maltesecross                  <font color="#993399">0x0af0</font>  <i><font color="#9A1900">/* U+2720 MALTESE CROSS */</font></i>
<b><font color="#000080">#define</font></b> XK_dagger                        <font color="#993399">0x0af1</font>  <i><font color="#9A1900">/* U+2020 DAGGER */</font></i>
<b><font color="#000080">#define</font></b> XK_doubledagger                  <font color="#993399">0x0af2</font>  <i><font color="#9A1900">/* U+2021 DOUBLE DAGGER */</font></i>
<b><font color="#000080">#define</font></b> XK_checkmark                     <font color="#993399">0x0af3</font>  <i><font color="#9A1900">/* U+2713 CHECK MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_ballotcross                   <font color="#993399">0x0af4</font>  <i><font color="#9A1900">/* U+2717 BALLOT X */</font></i>
<b><font color="#000080">#define</font></b> XK_musicalsharp                  <font color="#993399">0x0af5</font>  <i><font color="#9A1900">/* U+266F MUSIC SHARP SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_musicalflat                   <font color="#993399">0x0af6</font>  <i><font color="#9A1900">/* U+266D MUSIC FLAT SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_malesymbol                    <font color="#993399">0x0af7</font>  <i><font color="#9A1900">/* U+2642 MALE SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_femalesymbol                  <font color="#993399">0x0af8</font>  <i><font color="#9A1900">/* U+2640 FEMALE SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_telephone                     <font color="#993399">0x0af9</font>  <i><font color="#9A1900">/* U+260E BLACK TELEPHONE */</font></i>
<b><font color="#000080">#define</font></b> XK_telephonerecorder             <font color="#993399">0x0afa</font>  <i><font color="#9A1900">/* U+2315 TELEPHONE RECORDER */</font></i>
<b><font color="#000080">#define</font></b> XK_phonographcopyright           <font color="#993399">0x0afb</font>  <i><font color="#9A1900">/* U+2117 SOUND RECORDING COPYRIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_caret                         <font color="#993399">0x0afc</font>  <i><font color="#9A1900">/* U+2038 CARET */</font></i>
<b><font color="#000080">#define</font></b> XK_singlelowquotemark            <font color="#993399">0x0afd</font>  <i><font color="#9A1900">/* U+201A SINGLE LOW-9 QUOTATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_doublelowquotemark            <font color="#993399">0x0afe</font>  <i><font color="#9A1900">/* U+201E DOUBLE LOW-9 QUOTATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_cursor                        <font color="#993399">0x0aff</font>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_PUBLISHING */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * APL</font></i>
<i><font color="#9A1900"> * Byte 3 = 0x0b</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_APL
<b><font color="#000080">#define</font></b> XK_leftcaret                     <font color="#993399">0x0ba3</font>  <i><font color="#9A1900">/*(U+003C LESS-THAN SIGN)*/</font></i>
<b><font color="#000080">#define</font></b> XK_rightcaret                    <font color="#993399">0x0ba6</font>  <i><font color="#9A1900">/*(U+003E GREATER-THAN SIGN)*/</font></i>
<b><font color="#000080">#define</font></b> XK_downcaret                     <font color="#993399">0x0ba8</font>  <i><font color="#9A1900">/*(U+2228 LOGICAL OR)*/</font></i>
<b><font color="#000080">#define</font></b> XK_upcaret                       <font color="#993399">0x0ba9</font>  <i><font color="#9A1900">/*(U+2227 LOGICAL AND)*/</font></i>
<b><font color="#000080">#define</font></b> XK_overbar                       <font color="#993399">0x0bc0</font>  <i><font color="#9A1900">/*(U+00AF MACRON)*/</font></i>
<b><font color="#000080">#define</font></b> XK_downtack                      <font color="#993399">0x0bc2</font>  <i><font color="#9A1900">/* U+22A4 DOWN TACK */</font></i>
<b><font color="#000080">#define</font></b> XK_upshoe                        <font color="#993399">0x0bc3</font>  <i><font color="#9A1900">/*(U+2229 INTERSECTION)*/</font></i>
<b><font color="#000080">#define</font></b> XK_downstile                     <font color="#993399">0x0bc4</font>  <i><font color="#9A1900">/* U+230A LEFT FLOOR */</font></i>
<b><font color="#000080">#define</font></b> XK_underbar                      <font color="#993399">0x0bc6</font>  <i><font color="#9A1900">/*(U+005F LOW LINE)*/</font></i>
<b><font color="#000080">#define</font></b> XK_jot                           <font color="#993399">0x0bca</font>  <i><font color="#9A1900">/* U+2218 RING OPERATOR */</font></i>
<b><font color="#000080">#define</font></b> XK_quad                          <font color="#993399">0x0bcc</font>  <i><font color="#9A1900">/* U+2395 APL FUNCTIONAL SYMBOL QUAD */</font></i>
<b><font color="#000080">#define</font></b> XK_uptack                        <font color="#993399">0x0bce</font>  <i><font color="#9A1900">/* U+22A5 UP TACK */</font></i>
<b><font color="#000080">#define</font></b> XK_circle                        <font color="#993399">0x0bcf</font>  <i><font color="#9A1900">/* U+25CB WHITE CIRCLE */</font></i>
<b><font color="#000080">#define</font></b> XK_upstile                       <font color="#993399">0x0bd3</font>  <i><font color="#9A1900">/* U+2308 LEFT CEILING */</font></i>
<b><font color="#000080">#define</font></b> XK_downshoe                      <font color="#993399">0x0bd6</font>  <i><font color="#9A1900">/*(U+222A UNION)*/</font></i>
<b><font color="#000080">#define</font></b> XK_rightshoe                     <font color="#993399">0x0bd8</font>  <i><font color="#9A1900">/*(U+2283 SUPERSET OF)*/</font></i>
<b><font color="#000080">#define</font></b> XK_leftshoe                      <font color="#993399">0x0bda</font>  <i><font color="#9A1900">/*(U+2282 SUBSET OF)*/</font></i>
<b><font color="#000080">#define</font></b> XK_lefttack                      <font color="#993399">0x0bdc</font>  <i><font color="#9A1900">/* U+22A3 LEFT TACK */</font></i>
<b><font color="#000080">#define</font></b> XK_righttack                     <font color="#993399">0x0bfc</font>  <i><font color="#9A1900">/* U+22A2 RIGHT TACK */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_APL */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Hebrew</font></i>
<i><font color="#9A1900"> * Byte 3 = 0x0c</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_HEBREW
<b><font color="#000080">#define</font></b> XK_hebrew_doublelowline          <font color="#993399">0x0cdf</font>  <i><font color="#9A1900">/* U+2017 DOUBLE LOW LINE */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_aleph                  <font color="#993399">0x0ce0</font>  <i><font color="#9A1900">/* U+05D0 HEBREW LETTER ALEF */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_bet                    <font color="#993399">0x0ce1</font>  <i><font color="#9A1900">/* U+05D1 HEBREW LETTER BET */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_beth                   <font color="#993399">0x0ce1</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_gimel                  <font color="#993399">0x0ce2</font>  <i><font color="#9A1900">/* U+05D2 HEBREW LETTER GIMEL */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_gimmel                 <font color="#993399">0x0ce2</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_dalet                  <font color="#993399">0x0ce3</font>  <i><font color="#9A1900">/* U+05D3 HEBREW LETTER DALET */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_daleth                 <font color="#993399">0x0ce3</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_he                     <font color="#993399">0x0ce4</font>  <i><font color="#9A1900">/* U+05D4 HEBREW LETTER HE */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_waw                    <font color="#993399">0x0ce5</font>  <i><font color="#9A1900">/* U+05D5 HEBREW LETTER VAV */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_zain                   <font color="#993399">0x0ce6</font>  <i><font color="#9A1900">/* U+05D6 HEBREW LETTER ZAYIN */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_zayin                  <font color="#993399">0x0ce6</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_chet                   <font color="#993399">0x0ce7</font>  <i><font color="#9A1900">/* U+05D7 HEBREW LETTER HET */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_het                    <font color="#993399">0x0ce7</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_tet                    <font color="#993399">0x0ce8</font>  <i><font color="#9A1900">/* U+05D8 HEBREW LETTER TET */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_teth                   <font color="#993399">0x0ce8</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_yod                    <font color="#993399">0x0ce9</font>  <i><font color="#9A1900">/* U+05D9 HEBREW LETTER YOD */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_finalkaph              <font color="#993399">0x0cea</font>  <i><font color="#9A1900">/* U+05DA HEBREW LETTER FINAL KAF */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_kaph                   <font color="#993399">0x0ceb</font>  <i><font color="#9A1900">/* U+05DB HEBREW LETTER KAF */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_lamed                  <font color="#993399">0x0cec</font>  <i><font color="#9A1900">/* U+05DC HEBREW LETTER LAMED */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_finalmem               <font color="#993399">0x0ced</font>  <i><font color="#9A1900">/* U+05DD HEBREW LETTER FINAL MEM */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_mem                    <font color="#993399">0x0cee</font>  <i><font color="#9A1900">/* U+05DE HEBREW LETTER MEM */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_finalnun               <font color="#993399">0x0cef</font>  <i><font color="#9A1900">/* U+05DF HEBREW LETTER FINAL NUN */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_nun                    <font color="#993399">0x0cf0</font>  <i><font color="#9A1900">/* U+05E0 HEBREW LETTER NUN */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_samech                 <font color="#993399">0x0cf1</font>  <i><font color="#9A1900">/* U+05E1 HEBREW LETTER SAMEKH */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_samekh                 <font color="#993399">0x0cf1</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_ayin                   <font color="#993399">0x0cf2</font>  <i><font color="#9A1900">/* U+05E2 HEBREW LETTER AYIN */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_finalpe                <font color="#993399">0x0cf3</font>  <i><font color="#9A1900">/* U+05E3 HEBREW LETTER FINAL PE */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_pe                     <font color="#993399">0x0cf4</font>  <i><font color="#9A1900">/* U+05E4 HEBREW LETTER PE */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_finalzade              <font color="#993399">0x0cf5</font>  <i><font color="#9A1900">/* U+05E5 HEBREW LETTER FINAL TSADI */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_finalzadi              <font color="#993399">0x0cf5</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_zade                   <font color="#993399">0x0cf6</font>  <i><font color="#9A1900">/* U+05E6 HEBREW LETTER TSADI */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_zadi                   <font color="#993399">0x0cf6</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_qoph                   <font color="#993399">0x0cf7</font>  <i><font color="#9A1900">/* U+05E7 HEBREW LETTER QOF */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_kuf                    <font color="#993399">0x0cf7</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_resh                   <font color="#993399">0x0cf8</font>  <i><font color="#9A1900">/* U+05E8 HEBREW LETTER RESH */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_shin                   <font color="#993399">0x0cf9</font>  <i><font color="#9A1900">/* U+05E9 HEBREW LETTER SHIN */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_taw                    <font color="#993399">0x0cfa</font>  <i><font color="#9A1900">/* U+05EA HEBREW LETTER TAV */</font></i>
<b><font color="#000080">#define</font></b> XK_hebrew_taf                    <font color="#993399">0x0cfa</font>  <i><font color="#9A1900">/* deprecated */</font></i>
<b><font color="#000080">#define</font></b> XK_Hebrew_switch                 <font color="#993399">0xff7e</font>  <i><font color="#9A1900">/* Alias for mode_switch */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_HEBREW */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Thai</font></i>
<i><font color="#9A1900"> * Byte 3 = 0x0d</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_THAI
<b><font color="#000080">#define</font></b> XK_Thai_kokai                    <font color="#993399">0x0da1</font>  <i><font color="#9A1900">/* U+0E01 THAI CHARACTER KO KAI */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_khokhai                  <font color="#993399">0x0da2</font>  <i><font color="#9A1900">/* U+0E02 THAI CHARACTER KHO KHAI */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_khokhuat                 <font color="#993399">0x0da3</font>  <i><font color="#9A1900">/* U+0E03 THAI CHARACTER KHO KHUAT */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_khokhwai                 <font color="#993399">0x0da4</font>  <i><font color="#9A1900">/* U+0E04 THAI CHARACTER KHO KHWAI */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_khokhon                  <font color="#993399">0x0da5</font>  <i><font color="#9A1900">/* U+0E05 THAI CHARACTER KHO KHON */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_khorakhang               <font color="#993399">0x0da6</font>  <i><font color="#9A1900">/* U+0E06 THAI CHARACTER KHO RAKHANG */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_ngongu                   <font color="#993399">0x0da7</font>  <i><font color="#9A1900">/* U+0E07 THAI CHARACTER NGO NGU */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_chochan                  <font color="#993399">0x0da8</font>  <i><font color="#9A1900">/* U+0E08 THAI CHARACTER CHO CHAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_choching                 <font color="#993399">0x0da9</font>  <i><font color="#9A1900">/* U+0E09 THAI CHARACTER CHO CHING */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_chochang                 <font color="#993399">0x0daa</font>  <i><font color="#9A1900">/* U+0E0A THAI CHARACTER CHO CHANG */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_soso                     <font color="#993399">0x0dab</font>  <i><font color="#9A1900">/* U+0E0B THAI CHARACTER SO SO */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_chochoe                  <font color="#993399">0x0dac</font>  <i><font color="#9A1900">/* U+0E0C THAI CHARACTER CHO CHOE */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_yoying                   <font color="#993399">0x0dad</font>  <i><font color="#9A1900">/* U+0E0D THAI CHARACTER YO YING */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_dochada                  <font color="#993399">0x0dae</font>  <i><font color="#9A1900">/* U+0E0E THAI CHARACTER DO CHADA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_topatak                  <font color="#993399">0x0daf</font>  <i><font color="#9A1900">/* U+0E0F THAI CHARACTER TO PATAK */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_thothan                  <font color="#993399">0x0db0</font>  <i><font color="#9A1900">/* U+0E10 THAI CHARACTER THO THAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_thonangmontho            <font color="#993399">0x0db1</font>  <i><font color="#9A1900">/* U+0E11 THAI CHARACTER THO NANGMONTHO */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_thophuthao               <font color="#993399">0x0db2</font>  <i><font color="#9A1900">/* U+0E12 THAI CHARACTER THO PHUTHAO */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_nonen                    <font color="#993399">0x0db3</font>  <i><font color="#9A1900">/* U+0E13 THAI CHARACTER NO NEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_dodek                    <font color="#993399">0x0db4</font>  <i><font color="#9A1900">/* U+0E14 THAI CHARACTER DO DEK */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_totao                    <font color="#993399">0x0db5</font>  <i><font color="#9A1900">/* U+0E15 THAI CHARACTER TO TAO */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_thothung                 <font color="#993399">0x0db6</font>  <i><font color="#9A1900">/* U+0E16 THAI CHARACTER THO THUNG */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_thothahan                <font color="#993399">0x0db7</font>  <i><font color="#9A1900">/* U+0E17 THAI CHARACTER THO THAHAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_thothong                 <font color="#993399">0x0db8</font>  <i><font color="#9A1900">/* U+0E18 THAI CHARACTER THO THONG */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_nonu                     <font color="#993399">0x0db9</font>  <i><font color="#9A1900">/* U+0E19 THAI CHARACTER NO NU */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_bobaimai                 <font color="#993399">0x0dba</font>  <i><font color="#9A1900">/* U+0E1A THAI CHARACTER BO BAIMAI */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_popla                    <font color="#993399">0x0dbb</font>  <i><font color="#9A1900">/* U+0E1B THAI CHARACTER PO PLA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_phophung                 <font color="#993399">0x0dbc</font>  <i><font color="#9A1900">/* U+0E1C THAI CHARACTER PHO PHUNG */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_fofa                     <font color="#993399">0x0dbd</font>  <i><font color="#9A1900">/* U+0E1D THAI CHARACTER FO FA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_phophan                  <font color="#993399">0x0dbe</font>  <i><font color="#9A1900">/* U+0E1E THAI CHARACTER PHO PHAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_fofan                    <font color="#993399">0x0dbf</font>  <i><font color="#9A1900">/* U+0E1F THAI CHARACTER FO FAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_phosamphao               <font color="#993399">0x0dc0</font>  <i><font color="#9A1900">/* U+0E20 THAI CHARACTER PHO SAMPHAO */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_moma                     <font color="#993399">0x0dc1</font>  <i><font color="#9A1900">/* U+0E21 THAI CHARACTER MO MA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_yoyak                    <font color="#993399">0x0dc2</font>  <i><font color="#9A1900">/* U+0E22 THAI CHARACTER YO YAK */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_rorua                    <font color="#993399">0x0dc3</font>  <i><font color="#9A1900">/* U+0E23 THAI CHARACTER RO RUA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_ru                       <font color="#993399">0x0dc4</font>  <i><font color="#9A1900">/* U+0E24 THAI CHARACTER RU */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_loling                   <font color="#993399">0x0dc5</font>  <i><font color="#9A1900">/* U+0E25 THAI CHARACTER LO LING */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_lu                       <font color="#993399">0x0dc6</font>  <i><font color="#9A1900">/* U+0E26 THAI CHARACTER LU */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_wowaen                   <font color="#993399">0x0dc7</font>  <i><font color="#9A1900">/* U+0E27 THAI CHARACTER WO WAEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_sosala                   <font color="#993399">0x0dc8</font>  <i><font color="#9A1900">/* U+0E28 THAI CHARACTER SO SALA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_sorusi                   <font color="#993399">0x0dc9</font>  <i><font color="#9A1900">/* U+0E29 THAI CHARACTER SO RUSI */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_sosua                    <font color="#993399">0x0dca</font>  <i><font color="#9A1900">/* U+0E2A THAI CHARACTER SO SUA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_hohip                    <font color="#993399">0x0dcb</font>  <i><font color="#9A1900">/* U+0E2B THAI CHARACTER HO HIP */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_lochula                  <font color="#993399">0x0dcc</font>  <i><font color="#9A1900">/* U+0E2C THAI CHARACTER LO CHULA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_oang                     <font color="#993399">0x0dcd</font>  <i><font color="#9A1900">/* U+0E2D THAI CHARACTER O ANG */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_honokhuk                 <font color="#993399">0x0dce</font>  <i><font color="#9A1900">/* U+0E2E THAI CHARACTER HO NOKHUK */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_paiyannoi                <font color="#993399">0x0dcf</font>  <i><font color="#9A1900">/* U+0E2F THAI CHARACTER PAIYANNOI */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_saraa                    <font color="#993399">0x0dd0</font>  <i><font color="#9A1900">/* U+0E30 THAI CHARACTER SARA A */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_maihanakat               <font color="#993399">0x0dd1</font>  <i><font color="#9A1900">/* U+0E31 THAI CHARACTER MAI HAN-AKAT */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_saraaa                   <font color="#993399">0x0dd2</font>  <i><font color="#9A1900">/* U+0E32 THAI CHARACTER SARA AA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_saraam                   <font color="#993399">0x0dd3</font>  <i><font color="#9A1900">/* U+0E33 THAI CHARACTER SARA AM */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_sarai                    <font color="#993399">0x0dd4</font>  <i><font color="#9A1900">/* U+0E34 THAI CHARACTER SARA I */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_saraii                   <font color="#993399">0x0dd5</font>  <i><font color="#9A1900">/* U+0E35 THAI CHARACTER SARA II */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_saraue                   <font color="#993399">0x0dd6</font>  <i><font color="#9A1900">/* U+0E36 THAI CHARACTER SARA UE */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_sarauee                  <font color="#993399">0x0dd7</font>  <i><font color="#9A1900">/* U+0E37 THAI CHARACTER SARA UEE */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_sarau                    <font color="#993399">0x0dd8</font>  <i><font color="#9A1900">/* U+0E38 THAI CHARACTER SARA U */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_sarauu                   <font color="#993399">0x0dd9</font>  <i><font color="#9A1900">/* U+0E39 THAI CHARACTER SARA UU */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_phinthu                  <font color="#993399">0x0dda</font>  <i><font color="#9A1900">/* U+0E3A THAI CHARACTER PHINTHU */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_maihanakat_maitho        <font color="#993399">0x0dde</font>
<b><font color="#000080">#define</font></b> XK_Thai_baht                     <font color="#993399">0x0ddf</font>  <i><font color="#9A1900">/* U+0E3F THAI CURRENCY SYMBOL BAHT */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_sarae                    <font color="#993399">0x0de0</font>  <i><font color="#9A1900">/* U+0E40 THAI CHARACTER SARA E */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_saraae                   <font color="#993399">0x0de1</font>  <i><font color="#9A1900">/* U+0E41 THAI CHARACTER SARA AE */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_sarao                    <font color="#993399">0x0de2</font>  <i><font color="#9A1900">/* U+0E42 THAI CHARACTER SARA O */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_saraaimaimuan            <font color="#993399">0x0de3</font>  <i><font color="#9A1900">/* U+0E43 THAI CHARACTER SARA AI MAIMUAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_saraaimaimalai           <font color="#993399">0x0de4</font>  <i><font color="#9A1900">/* U+0E44 THAI CHARACTER SARA AI MAIMALAI */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_lakkhangyao              <font color="#993399">0x0de5</font>  <i><font color="#9A1900">/* U+0E45 THAI CHARACTER LAKKHANGYAO */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_maiyamok                 <font color="#993399">0x0de6</font>  <i><font color="#9A1900">/* U+0E46 THAI CHARACTER MAIYAMOK */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_maitaikhu                <font color="#993399">0x0de7</font>  <i><font color="#9A1900">/* U+0E47 THAI CHARACTER MAITAIKHU */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_maiek                    <font color="#993399">0x0de8</font>  <i><font color="#9A1900">/* U+0E48 THAI CHARACTER MAI EK */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_maitho                   <font color="#993399">0x0de9</font>  <i><font color="#9A1900">/* U+0E49 THAI CHARACTER MAI THO */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_maitri                   <font color="#993399">0x0dea</font>  <i><font color="#9A1900">/* U+0E4A THAI CHARACTER MAI TRI */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_maichattawa              <font color="#993399">0x0deb</font>  <i><font color="#9A1900">/* U+0E4B THAI CHARACTER MAI CHATTAWA */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_thanthakhat              <font color="#993399">0x0dec</font>  <i><font color="#9A1900">/* U+0E4C THAI CHARACTER THANTHAKHAT */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_nikhahit                 <font color="#993399">0x0ded</font>  <i><font color="#9A1900">/* U+0E4D THAI CHARACTER NIKHAHIT */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_leksun                   <font color="#993399">0x0df0</font>  <i><font color="#9A1900">/* U+0E50 THAI DIGIT ZERO */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_leknung                  <font color="#993399">0x0df1</font>  <i><font color="#9A1900">/* U+0E51 THAI DIGIT ONE */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_leksong                  <font color="#993399">0x0df2</font>  <i><font color="#9A1900">/* U+0E52 THAI DIGIT TWO */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_leksam                   <font color="#993399">0x0df3</font>  <i><font color="#9A1900">/* U+0E53 THAI DIGIT THREE */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_leksi                    <font color="#993399">0x0df4</font>  <i><font color="#9A1900">/* U+0E54 THAI DIGIT FOUR */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_lekha                    <font color="#993399">0x0df5</font>  <i><font color="#9A1900">/* U+0E55 THAI DIGIT FIVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_lekhok                   <font color="#993399">0x0df6</font>  <i><font color="#9A1900">/* U+0E56 THAI DIGIT SIX */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_lekchet                  <font color="#993399">0x0df7</font>  <i><font color="#9A1900">/* U+0E57 THAI DIGIT SEVEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_lekpaet                  <font color="#993399">0x0df8</font>  <i><font color="#9A1900">/* U+0E58 THAI DIGIT EIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_Thai_lekkao                   <font color="#993399">0x0df9</font>  <i><font color="#9A1900">/* U+0E59 THAI DIGIT NINE */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_THAI */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Korean</font></i>
<i><font color="#9A1900"> * Byte 3 = 0x0e</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_KOREAN

<b><font color="#000080">#define</font></b> XK_Hangul                        <font color="#993399">0xff31</font>  <i><font color="#9A1900">/* Hangul start/stop(toggle) */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_Start                  <font color="#993399">0xff32</font>  <i><font color="#9A1900">/* Hangul start */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_End                    <font color="#993399">0xff33</font>  <i><font color="#9A1900">/* Hangul end, English start */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_Hanja                  <font color="#993399">0xff34</font>  <i><font color="#9A1900">/* Start Hangul-&gt;Hanja Conversion */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_Jamo                   <font color="#993399">0xff35</font>  <i><font color="#9A1900">/* Hangul Jamo mode */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_Romaja                 <font color="#993399">0xff36</font>  <i><font color="#9A1900">/* Hangul Romaja mode */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_Codeinput              <font color="#993399">0xff37</font>  <i><font color="#9A1900">/* Hangul code input mode */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_Jeonja                 <font color="#993399">0xff38</font>  <i><font color="#9A1900">/* Jeonja mode */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_Banja                  <font color="#993399">0xff39</font>  <i><font color="#9A1900">/* Banja mode */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_PreHanja               <font color="#993399">0xff3a</font>  <i><font color="#9A1900">/* Pre Hanja conversion */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_PostHanja              <font color="#993399">0xff3b</font>  <i><font color="#9A1900">/* Post Hanja conversion */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_SingleCandidate        <font color="#993399">0xff3c</font>  <i><font color="#9A1900">/* Single candidate */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_MultipleCandidate      <font color="#993399">0xff3d</font>  <i><font color="#9A1900">/* Multiple candidate */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_PreviousCandidate      <font color="#993399">0xff3e</font>  <i><font color="#9A1900">/* Previous candidate */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_Special                <font color="#993399">0xff3f</font>  <i><font color="#9A1900">/* Special symbols */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_switch                 <font color="#993399">0xff7e</font>  <i><font color="#9A1900">/* Alias for mode_switch */</font></i>

<i><font color="#9A1900">/* Hangul Consonant Characters */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_Kiyeog                 <font color="#993399">0x0ea1</font>
<b><font color="#000080">#define</font></b> XK_Hangul_SsangKiyeog            <font color="#993399">0x0ea2</font>
<b><font color="#000080">#define</font></b> XK_Hangul_KiyeogSios             <font color="#993399">0x0ea3</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Nieun                  <font color="#993399">0x0ea4</font>
<b><font color="#000080">#define</font></b> XK_Hangul_NieunJieuj             <font color="#993399">0x0ea5</font>
<b><font color="#000080">#define</font></b> XK_Hangul_NieunHieuh             <font color="#993399">0x0ea6</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Dikeud                 <font color="#993399">0x0ea7</font>
<b><font color="#000080">#define</font></b> XK_Hangul_SsangDikeud            <font color="#993399">0x0ea8</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Rieul                  <font color="#993399">0x0ea9</font>
<b><font color="#000080">#define</font></b> XK_Hangul_RieulKiyeog            <font color="#993399">0x0eaa</font>
<b><font color="#000080">#define</font></b> XK_Hangul_RieulMieum             <font color="#993399">0x0eab</font>
<b><font color="#000080">#define</font></b> XK_Hangul_RieulPieub             <font color="#993399">0x0eac</font>
<b><font color="#000080">#define</font></b> XK_Hangul_RieulSios              <font color="#993399">0x0ead</font>
<b><font color="#000080">#define</font></b> XK_Hangul_RieulTieut             <font color="#993399">0x0eae</font>
<b><font color="#000080">#define</font></b> XK_Hangul_RieulPhieuf            <font color="#993399">0x0eaf</font>
<b><font color="#000080">#define</font></b> XK_Hangul_RieulHieuh             <font color="#993399">0x0eb0</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Mieum                  <font color="#993399">0x0eb1</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Pieub                  <font color="#993399">0x0eb2</font>
<b><font color="#000080">#define</font></b> XK_Hangul_SsangPieub             <font color="#993399">0x0eb3</font>
<b><font color="#000080">#define</font></b> XK_Hangul_PieubSios              <font color="#993399">0x0eb4</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Sios                   <font color="#993399">0x0eb5</font>
<b><font color="#000080">#define</font></b> XK_Hangul_SsangSios              <font color="#993399">0x0eb6</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Ieung                  <font color="#993399">0x0eb7</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Jieuj                  <font color="#993399">0x0eb8</font>
<b><font color="#000080">#define</font></b> XK_Hangul_SsangJieuj             <font color="#993399">0x0eb9</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Cieuc                  <font color="#993399">0x0eba</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Khieuq                 <font color="#993399">0x0ebb</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Tieut                  <font color="#993399">0x0ebc</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Phieuf                 <font color="#993399">0x0ebd</font>
<b><font color="#000080">#define</font></b> XK_Hangul_Hieuh                  <font color="#993399">0x0ebe</font>

<i><font color="#9A1900">/* Hangul Vowel Characters */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_A                      <font color="#993399">0x0ebf</font>
<b><font color="#000080">#define</font></b> XK_Hangul_AE                     <font color="#993399">0x0ec0</font>
<b><font color="#000080">#define</font></b> XK_Hangul_YA                     <font color="#993399">0x0ec1</font>
<b><font color="#000080">#define</font></b> XK_Hangul_YAE                    <font color="#993399">0x0ec2</font>
<b><font color="#000080">#define</font></b> XK_Hangul_EO                     <font color="#993399">0x0ec3</font>
<b><font color="#000080">#define</font></b> XK_Hangul_E                      <font color="#993399">0x0ec4</font>
<b><font color="#000080">#define</font></b> XK_Hangul_YEO                    <font color="#993399">0x0ec5</font>
<b><font color="#000080">#define</font></b> XK_Hangul_YE                     <font color="#993399">0x0ec6</font>
<b><font color="#000080">#define</font></b> XK_Hangul_O                      <font color="#993399">0x0ec7</font>
<b><font color="#000080">#define</font></b> XK_Hangul_WA                     <font color="#993399">0x0ec8</font>
<b><font color="#000080">#define</font></b> XK_Hangul_WAE                    <font color="#993399">0x0ec9</font>
<b><font color="#000080">#define</font></b> XK_Hangul_OE                     <font color="#993399">0x0eca</font>
<b><font color="#000080">#define</font></b> XK_Hangul_YO                     <font color="#993399">0x0ecb</font>
<b><font color="#000080">#define</font></b> XK_Hangul_U                      <font color="#993399">0x0ecc</font>
<b><font color="#000080">#define</font></b> XK_Hangul_WEO                    <font color="#993399">0x0ecd</font>
<b><font color="#000080">#define</font></b> XK_Hangul_WE                     <font color="#993399">0x0ece</font>
<b><font color="#000080">#define</font></b> XK_Hangul_WI                     <font color="#993399">0x0ecf</font>
<b><font color="#000080">#define</font></b> XK_Hangul_YU                     <font color="#993399">0x0ed0</font>
<b><font color="#000080">#define</font></b> XK_Hangul_EU                     <font color="#993399">0x0ed1</font>
<b><font color="#000080">#define</font></b> XK_Hangul_YI                     <font color="#993399">0x0ed2</font>
<b><font color="#000080">#define</font></b> XK_Hangul_I                      <font color="#993399">0x0ed3</font>

<i><font color="#9A1900">/* Hangul syllable-final (JongSeong) Characters */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Kiyeog               <font color="#993399">0x0ed4</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_SsangKiyeog          <font color="#993399">0x0ed5</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_KiyeogSios           <font color="#993399">0x0ed6</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Nieun                <font color="#993399">0x0ed7</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_NieunJieuj           <font color="#993399">0x0ed8</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_NieunHieuh           <font color="#993399">0x0ed9</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Dikeud               <font color="#993399">0x0eda</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Rieul                <font color="#993399">0x0edb</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_RieulKiyeog          <font color="#993399">0x0edc</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_RieulMieum           <font color="#993399">0x0edd</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_RieulPieub           <font color="#993399">0x0ede</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_RieulSios            <font color="#993399">0x0edf</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_RieulTieut           <font color="#993399">0x0ee0</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_RieulPhieuf          <font color="#993399">0x0ee1</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_RieulHieuh           <font color="#993399">0x0ee2</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Mieum                <font color="#993399">0x0ee3</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Pieub                <font color="#993399">0x0ee4</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_PieubSios            <font color="#993399">0x0ee5</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Sios                 <font color="#993399">0x0ee6</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_SsangSios            <font color="#993399">0x0ee7</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Ieung                <font color="#993399">0x0ee8</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Jieuj                <font color="#993399">0x0ee9</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Cieuc                <font color="#993399">0x0eea</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Khieuq               <font color="#993399">0x0eeb</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Tieut                <font color="#993399">0x0eec</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Phieuf               <font color="#993399">0x0eed</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_Hieuh                <font color="#993399">0x0eee</font>

<i><font color="#9A1900">/* Ancient Hangul Consonant Characters */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_RieulYeorinHieuh       <font color="#993399">0x0eef</font>
<b><font color="#000080">#define</font></b> XK_Hangul_SunkyeongeumMieum      <font color="#993399">0x0ef0</font>
<b><font color="#000080">#define</font></b> XK_Hangul_SunkyeongeumPieub      <font color="#993399">0x0ef1</font>
<b><font color="#000080">#define</font></b> XK_Hangul_PanSios                <font color="#993399">0x0ef2</font>
<b><font color="#000080">#define</font></b> XK_Hangul_KkogjiDalrinIeung      <font color="#993399">0x0ef3</font>
<b><font color="#000080">#define</font></b> XK_Hangul_SunkyeongeumPhieuf     <font color="#993399">0x0ef4</font>
<b><font color="#000080">#define</font></b> XK_Hangul_YeorinHieuh            <font color="#993399">0x0ef5</font>

<i><font color="#9A1900">/* Ancient Hangul Vowel Characters */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_AraeA                  <font color="#993399">0x0ef6</font>
<b><font color="#000080">#define</font></b> XK_Hangul_AraeAE                 <font color="#993399">0x0ef7</font>

<i><font color="#9A1900">/* Ancient Hangul syllable-final (JongSeong) Characters */</font></i>
<b><font color="#000080">#define</font></b> XK_Hangul_J_PanSios              <font color="#993399">0x0ef8</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_KkogjiDalrinIeung    <font color="#993399">0x0ef9</font>
<b><font color="#000080">#define</font></b> XK_Hangul_J_YeorinHieuh          <font color="#993399">0x0efa</font>

<i><font color="#9A1900">/* Korean currency symbol */</font></i>
<b><font color="#000080">#define</font></b> XK_Korean_Won                    <font color="#993399">0x0eff</font>  <i><font color="#9A1900">/*(U+20A9 WON SIGN)*/</font></i>

<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_KOREAN */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Armenian</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_ARMENIAN
<b><font color="#000080">#define</font></b> XK_Armenian_ligature_ew       <font color="#993399">0x1000587</font>  <i><font color="#9A1900">/* U+0587 ARMENIAN SMALL LIGATURE ECH YIWN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_full_stop         <font color="#993399">0x1000589</font>  <i><font color="#9A1900">/* U+0589 ARMENIAN FULL STOP */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_verjaket          <font color="#993399">0x1000589</font>  <i><font color="#9A1900">/* U+0589 ARMENIAN FULL STOP */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_separation_mark   <font color="#993399">0x100055d</font>  <i><font color="#9A1900">/* U+055D ARMENIAN COMMA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_but               <font color="#993399">0x100055d</font>  <i><font color="#9A1900">/* U+055D ARMENIAN COMMA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_hyphen            <font color="#993399">0x100058a</font>  <i><font color="#9A1900">/* U+058A ARMENIAN HYPHEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_yentamna          <font color="#993399">0x100058a</font>  <i><font color="#9A1900">/* U+058A ARMENIAN HYPHEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_exclam            <font color="#993399">0x100055c</font>  <i><font color="#9A1900">/* U+055C ARMENIAN EXCLAMATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_amanak            <font color="#993399">0x100055c</font>  <i><font color="#9A1900">/* U+055C ARMENIAN EXCLAMATION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_accent            <font color="#993399">0x100055b</font>  <i><font color="#9A1900">/* U+055B ARMENIAN EMPHASIS MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_shesht            <font color="#993399">0x100055b</font>  <i><font color="#9A1900">/* U+055B ARMENIAN EMPHASIS MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_question          <font color="#993399">0x100055e</font>  <i><font color="#9A1900">/* U+055E ARMENIAN QUESTION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_paruyk            <font color="#993399">0x100055e</font>  <i><font color="#9A1900">/* U+055E ARMENIAN QUESTION MARK */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_AYB               <font color="#993399">0x1000531</font>  <i><font color="#9A1900">/* U+0531 ARMENIAN CAPITAL LETTER AYB */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ayb               <font color="#993399">0x1000561</font>  <i><font color="#9A1900">/* U+0561 ARMENIAN SMALL LETTER AYB */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_BEN               <font color="#993399">0x1000532</font>  <i><font color="#9A1900">/* U+0532 ARMENIAN CAPITAL LETTER BEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ben               <font color="#993399">0x1000562</font>  <i><font color="#9A1900">/* U+0562 ARMENIAN SMALL LETTER BEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_GIM               <font color="#993399">0x1000533</font>  <i><font color="#9A1900">/* U+0533 ARMENIAN CAPITAL LETTER GIM */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_gim               <font color="#993399">0x1000563</font>  <i><font color="#9A1900">/* U+0563 ARMENIAN SMALL LETTER GIM */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_DA                <font color="#993399">0x1000534</font>  <i><font color="#9A1900">/* U+0534 ARMENIAN CAPITAL LETTER DA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_da                <font color="#993399">0x1000564</font>  <i><font color="#9A1900">/* U+0564 ARMENIAN SMALL LETTER DA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_YECH              <font color="#993399">0x1000535</font>  <i><font color="#9A1900">/* U+0535 ARMENIAN CAPITAL LETTER ECH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_yech              <font color="#993399">0x1000565</font>  <i><font color="#9A1900">/* U+0565 ARMENIAN SMALL LETTER ECH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ZA                <font color="#993399">0x1000536</font>  <i><font color="#9A1900">/* U+0536 ARMENIAN CAPITAL LETTER ZA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_za                <font color="#993399">0x1000566</font>  <i><font color="#9A1900">/* U+0566 ARMENIAN SMALL LETTER ZA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_E                 <font color="#993399">0x1000537</font>  <i><font color="#9A1900">/* U+0537 ARMENIAN CAPITAL LETTER EH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_e                 <font color="#993399">0x1000567</font>  <i><font color="#9A1900">/* U+0567 ARMENIAN SMALL LETTER EH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_AT                <font color="#993399">0x1000538</font>  <i><font color="#9A1900">/* U+0538 ARMENIAN CAPITAL LETTER ET */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_at                <font color="#993399">0x1000568</font>  <i><font color="#9A1900">/* U+0568 ARMENIAN SMALL LETTER ET */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_TO                <font color="#993399">0x1000539</font>  <i><font color="#9A1900">/* U+0539 ARMENIAN CAPITAL LETTER TO */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_to                <font color="#993399">0x1000569</font>  <i><font color="#9A1900">/* U+0569 ARMENIAN SMALL LETTER TO */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ZHE               <font color="#993399">0x100053a</font>  <i><font color="#9A1900">/* U+053A ARMENIAN CAPITAL LETTER ZHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_zhe               <font color="#993399">0x100056a</font>  <i><font color="#9A1900">/* U+056A ARMENIAN SMALL LETTER ZHE */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_INI               <font color="#993399">0x100053b</font>  <i><font color="#9A1900">/* U+053B ARMENIAN CAPITAL LETTER INI */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ini               <font color="#993399">0x100056b</font>  <i><font color="#9A1900">/* U+056B ARMENIAN SMALL LETTER INI */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_LYUN              <font color="#993399">0x100053c</font>  <i><font color="#9A1900">/* U+053C ARMENIAN CAPITAL LETTER LIWN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_lyun              <font color="#993399">0x100056c</font>  <i><font color="#9A1900">/* U+056C ARMENIAN SMALL LETTER LIWN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_KHE               <font color="#993399">0x100053d</font>  <i><font color="#9A1900">/* U+053D ARMENIAN CAPITAL LETTER XEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_khe               <font color="#993399">0x100056d</font>  <i><font color="#9A1900">/* U+056D ARMENIAN SMALL LETTER XEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_TSA               <font color="#993399">0x100053e</font>  <i><font color="#9A1900">/* U+053E ARMENIAN CAPITAL LETTER CA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_tsa               <font color="#993399">0x100056e</font>  <i><font color="#9A1900">/* U+056E ARMENIAN SMALL LETTER CA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_KEN               <font color="#993399">0x100053f</font>  <i><font color="#9A1900">/* U+053F ARMENIAN CAPITAL LETTER KEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ken               <font color="#993399">0x100056f</font>  <i><font color="#9A1900">/* U+056F ARMENIAN SMALL LETTER KEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_HO                <font color="#993399">0x1000540</font>  <i><font color="#9A1900">/* U+0540 ARMENIAN CAPITAL LETTER HO */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ho                <font color="#993399">0x1000570</font>  <i><font color="#9A1900">/* U+0570 ARMENIAN SMALL LETTER HO */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_DZA               <font color="#993399">0x1000541</font>  <i><font color="#9A1900">/* U+0541 ARMENIAN CAPITAL LETTER JA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_dza               <font color="#993399">0x1000571</font>  <i><font color="#9A1900">/* U+0571 ARMENIAN SMALL LETTER JA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_GHAT              <font color="#993399">0x1000542</font>  <i><font color="#9A1900">/* U+0542 ARMENIAN CAPITAL LETTER GHAD */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ghat              <font color="#993399">0x1000572</font>  <i><font color="#9A1900">/* U+0572 ARMENIAN SMALL LETTER GHAD */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_TCHE              <font color="#993399">0x1000543</font>  <i><font color="#9A1900">/* U+0543 ARMENIAN CAPITAL LETTER CHEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_tche              <font color="#993399">0x1000573</font>  <i><font color="#9A1900">/* U+0573 ARMENIAN SMALL LETTER CHEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_MEN               <font color="#993399">0x1000544</font>  <i><font color="#9A1900">/* U+0544 ARMENIAN CAPITAL LETTER MEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_men               <font color="#993399">0x1000574</font>  <i><font color="#9A1900">/* U+0574 ARMENIAN SMALL LETTER MEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_HI                <font color="#993399">0x1000545</font>  <i><font color="#9A1900">/* U+0545 ARMENIAN CAPITAL LETTER YI */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_hi                <font color="#993399">0x1000575</font>  <i><font color="#9A1900">/* U+0575 ARMENIAN SMALL LETTER YI */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_NU                <font color="#993399">0x1000546</font>  <i><font color="#9A1900">/* U+0546 ARMENIAN CAPITAL LETTER NOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_nu                <font color="#993399">0x1000576</font>  <i><font color="#9A1900">/* U+0576 ARMENIAN SMALL LETTER NOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_SHA               <font color="#993399">0x1000547</font>  <i><font color="#9A1900">/* U+0547 ARMENIAN CAPITAL LETTER SHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_sha               <font color="#993399">0x1000577</font>  <i><font color="#9A1900">/* U+0577 ARMENIAN SMALL LETTER SHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_VO                <font color="#993399">0x1000548</font>  <i><font color="#9A1900">/* U+0548 ARMENIAN CAPITAL LETTER VO */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_vo                <font color="#993399">0x1000578</font>  <i><font color="#9A1900">/* U+0578 ARMENIAN SMALL LETTER VO */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_CHA               <font color="#993399">0x1000549</font>  <i><font color="#9A1900">/* U+0549 ARMENIAN CAPITAL LETTER CHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_cha               <font color="#993399">0x1000579</font>  <i><font color="#9A1900">/* U+0579 ARMENIAN SMALL LETTER CHA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_PE                <font color="#993399">0x100054a</font>  <i><font color="#9A1900">/* U+054A ARMENIAN CAPITAL LETTER PEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_pe                <font color="#993399">0x100057a</font>  <i><font color="#9A1900">/* U+057A ARMENIAN SMALL LETTER PEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_JE                <font color="#993399">0x100054b</font>  <i><font color="#9A1900">/* U+054B ARMENIAN CAPITAL LETTER JHEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_je                <font color="#993399">0x100057b</font>  <i><font color="#9A1900">/* U+057B ARMENIAN SMALL LETTER JHEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_RA                <font color="#993399">0x100054c</font>  <i><font color="#9A1900">/* U+054C ARMENIAN CAPITAL LETTER RA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ra                <font color="#993399">0x100057c</font>  <i><font color="#9A1900">/* U+057C ARMENIAN SMALL LETTER RA */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_SE                <font color="#993399">0x100054d</font>  <i><font color="#9A1900">/* U+054D ARMENIAN CAPITAL LETTER SEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_se                <font color="#993399">0x100057d</font>  <i><font color="#9A1900">/* U+057D ARMENIAN SMALL LETTER SEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_VEV               <font color="#993399">0x100054e</font>  <i><font color="#9A1900">/* U+054E ARMENIAN CAPITAL LETTER VEW */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_vev               <font color="#993399">0x100057e</font>  <i><font color="#9A1900">/* U+057E ARMENIAN SMALL LETTER VEW */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_TYUN              <font color="#993399">0x100054f</font>  <i><font color="#9A1900">/* U+054F ARMENIAN CAPITAL LETTER TIWN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_tyun              <font color="#993399">0x100057f</font>  <i><font color="#9A1900">/* U+057F ARMENIAN SMALL LETTER TIWN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_RE                <font color="#993399">0x1000550</font>  <i><font color="#9A1900">/* U+0550 ARMENIAN CAPITAL LETTER REH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_re                <font color="#993399">0x1000580</font>  <i><font color="#9A1900">/* U+0580 ARMENIAN SMALL LETTER REH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_TSO               <font color="#993399">0x1000551</font>  <i><font color="#9A1900">/* U+0551 ARMENIAN CAPITAL LETTER CO */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_tso               <font color="#993399">0x1000581</font>  <i><font color="#9A1900">/* U+0581 ARMENIAN SMALL LETTER CO */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_VYUN              <font color="#993399">0x1000552</font>  <i><font color="#9A1900">/* U+0552 ARMENIAN CAPITAL LETTER YIWN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_vyun              <font color="#993399">0x1000582</font>  <i><font color="#9A1900">/* U+0582 ARMENIAN SMALL LETTER YIWN */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_PYUR              <font color="#993399">0x1000553</font>  <i><font color="#9A1900">/* U+0553 ARMENIAN CAPITAL LETTER PIWR */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_pyur              <font color="#993399">0x1000583</font>  <i><font color="#9A1900">/* U+0583 ARMENIAN SMALL LETTER PIWR */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_KE                <font color="#993399">0x1000554</font>  <i><font color="#9A1900">/* U+0554 ARMENIAN CAPITAL LETTER KEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_ke                <font color="#993399">0x1000584</font>  <i><font color="#9A1900">/* U+0584 ARMENIAN SMALL LETTER KEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_O                 <font color="#993399">0x1000555</font>  <i><font color="#9A1900">/* U+0555 ARMENIAN CAPITAL LETTER OH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_o                 <font color="#993399">0x1000585</font>  <i><font color="#9A1900">/* U+0585 ARMENIAN SMALL LETTER OH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_FE                <font color="#993399">0x1000556</font>  <i><font color="#9A1900">/* U+0556 ARMENIAN CAPITAL LETTER FEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_fe                <font color="#993399">0x1000586</font>  <i><font color="#9A1900">/* U+0586 ARMENIAN SMALL LETTER FEH */</font></i>
<b><font color="#000080">#define</font></b> XK_Armenian_apostrophe        <font color="#993399">0x100055a</font>  <i><font color="#9A1900">/* U+055A ARMENIAN APOSTROPHE */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_ARMENIAN */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Georgian</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_GEORGIAN
<b><font color="#000080">#define</font></b> XK_Georgian_an                <font color="#993399">0x10010d0</font>  <i><font color="#9A1900">/* U+10D0 GEORGIAN LETTER AN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_ban               <font color="#993399">0x10010d1</font>  <i><font color="#9A1900">/* U+10D1 GEORGIAN LETTER BAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_gan               <font color="#993399">0x10010d2</font>  <i><font color="#9A1900">/* U+10D2 GEORGIAN LETTER GAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_don               <font color="#993399">0x10010d3</font>  <i><font color="#9A1900">/* U+10D3 GEORGIAN LETTER DON */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_en                <font color="#993399">0x10010d4</font>  <i><font color="#9A1900">/* U+10D4 GEORGIAN LETTER EN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_vin               <font color="#993399">0x10010d5</font>  <i><font color="#9A1900">/* U+10D5 GEORGIAN LETTER VIN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_zen               <font color="#993399">0x10010d6</font>  <i><font color="#9A1900">/* U+10D6 GEORGIAN LETTER ZEN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_tan               <font color="#993399">0x10010d7</font>  <i><font color="#9A1900">/* U+10D7 GEORGIAN LETTER TAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_in                <font color="#993399">0x10010d8</font>  <i><font color="#9A1900">/* U+10D8 GEORGIAN LETTER IN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_kan               <font color="#993399">0x10010d9</font>  <i><font color="#9A1900">/* U+10D9 GEORGIAN LETTER KAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_las               <font color="#993399">0x10010da</font>  <i><font color="#9A1900">/* U+10DA GEORGIAN LETTER LAS */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_man               <font color="#993399">0x10010db</font>  <i><font color="#9A1900">/* U+10DB GEORGIAN LETTER MAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_nar               <font color="#993399">0x10010dc</font>  <i><font color="#9A1900">/* U+10DC GEORGIAN LETTER NAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_on                <font color="#993399">0x10010dd</font>  <i><font color="#9A1900">/* U+10DD GEORGIAN LETTER ON */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_par               <font color="#993399">0x10010de</font>  <i><font color="#9A1900">/* U+10DE GEORGIAN LETTER PAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_zhar              <font color="#993399">0x10010df</font>  <i><font color="#9A1900">/* U+10DF GEORGIAN LETTER ZHAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_rae               <font color="#993399">0x10010e0</font>  <i><font color="#9A1900">/* U+10E0 GEORGIAN LETTER RAE */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_san               <font color="#993399">0x10010e1</font>  <i><font color="#9A1900">/* U+10E1 GEORGIAN LETTER SAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_tar               <font color="#993399">0x10010e2</font>  <i><font color="#9A1900">/* U+10E2 GEORGIAN LETTER TAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_un                <font color="#993399">0x10010e3</font>  <i><font color="#9A1900">/* U+10E3 GEORGIAN LETTER UN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_phar              <font color="#993399">0x10010e4</font>  <i><font color="#9A1900">/* U+10E4 GEORGIAN LETTER PHAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_khar              <font color="#993399">0x10010e5</font>  <i><font color="#9A1900">/* U+10E5 GEORGIAN LETTER KHAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_ghan              <font color="#993399">0x10010e6</font>  <i><font color="#9A1900">/* U+10E6 GEORGIAN LETTER GHAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_qar               <font color="#993399">0x10010e7</font>  <i><font color="#9A1900">/* U+10E7 GEORGIAN LETTER QAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_shin              <font color="#993399">0x10010e8</font>  <i><font color="#9A1900">/* U+10E8 GEORGIAN LETTER SHIN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_chin              <font color="#993399">0x10010e9</font>  <i><font color="#9A1900">/* U+10E9 GEORGIAN LETTER CHIN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_can               <font color="#993399">0x10010ea</font>  <i><font color="#9A1900">/* U+10EA GEORGIAN LETTER CAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_jil               <font color="#993399">0x10010eb</font>  <i><font color="#9A1900">/* U+10EB GEORGIAN LETTER JIL */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_cil               <font color="#993399">0x10010ec</font>  <i><font color="#9A1900">/* U+10EC GEORGIAN LETTER CIL */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_char              <font color="#993399">0x10010ed</font>  <i><font color="#9A1900">/* U+10ED GEORGIAN LETTER CHAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_xan               <font color="#993399">0x10010ee</font>  <i><font color="#9A1900">/* U+10EE GEORGIAN LETTER XAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_jhan              <font color="#993399">0x10010ef</font>  <i><font color="#9A1900">/* U+10EF GEORGIAN LETTER JHAN */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_hae               <font color="#993399">0x10010f0</font>  <i><font color="#9A1900">/* U+10F0 GEORGIAN LETTER HAE */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_he                <font color="#993399">0x10010f1</font>  <i><font color="#9A1900">/* U+10F1 GEORGIAN LETTER HE */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_hie               <font color="#993399">0x10010f2</font>  <i><font color="#9A1900">/* U+10F2 GEORGIAN LETTER HIE */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_we                <font color="#993399">0x10010f3</font>  <i><font color="#9A1900">/* U+10F3 GEORGIAN LETTER WE */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_har               <font color="#993399">0x10010f4</font>  <i><font color="#9A1900">/* U+10F4 GEORGIAN LETTER HAR */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_hoe               <font color="#993399">0x10010f5</font>  <i><font color="#9A1900">/* U+10F5 GEORGIAN LETTER HOE */</font></i>
<b><font color="#000080">#define</font></b> XK_Georgian_fi                <font color="#993399">0x10010f6</font>  <i><font color="#9A1900">/* U+10F6 GEORGIAN LETTER FI */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_GEORGIAN */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Azeri (and other Turkic or Caucasian languages)</font></i>
<i><font color="#9A1900"> */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_CAUCASUS
<i><font color="#9A1900">/* latin */</font></i>
<b><font color="#000080">#define</font></b> XK_Xabovedot                  <font color="#993399">0x1001e8a</font>  <i><font color="#9A1900">/* U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ibreve                     <font color="#993399">0x100012c</font>  <i><font color="#9A1900">/* U+012C LATIN CAPITAL LETTER I WITH BREVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Zstroke                    <font color="#993399">0x10001b5</font>  <i><font color="#9A1900">/* U+01B5 LATIN CAPITAL LETTER Z WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_Gcaron                     <font color="#993399">0x10001e6</font>  <i><font color="#9A1900">/* U+01E6 LATIN CAPITAL LETTER G WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Ocaron                     <font color="#993399">0x10001d1</font>  <i><font color="#9A1900">/* U+01D2 LATIN CAPITAL LETTER O WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_Obarred                    <font color="#993399">0x100019f</font>  <i><font color="#9A1900">/* U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_xabovedot                  <font color="#993399">0x1001e8b</font>  <i><font color="#9A1900">/* U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ibreve                     <font color="#993399">0x100012d</font>  <i><font color="#9A1900">/* U+012D LATIN SMALL LETTER I WITH BREVE */</font></i>
<b><font color="#000080">#define</font></b> XK_zstroke                    <font color="#993399">0x10001b6</font>  <i><font color="#9A1900">/* U+01B6 LATIN SMALL LETTER Z WITH STROKE */</font></i>
<b><font color="#000080">#define</font></b> XK_gcaron                     <font color="#993399">0x10001e7</font>  <i><font color="#9A1900">/* U+01E7 LATIN SMALL LETTER G WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_ocaron                     <font color="#993399">0x10001d2</font>  <i><font color="#9A1900">/* U+01D2 LATIN SMALL LETTER O WITH CARON */</font></i>
<b><font color="#000080">#define</font></b> XK_obarred                    <font color="#993399">0x1000275</font>  <i><font color="#9A1900">/* U+0275 LATIN SMALL LETTER BARRED O */</font></i>
<b><font color="#000080">#define</font></b> XK_SCHWA                      <font color="#993399">0x100018f</font>  <i><font color="#9A1900">/* U+018F LATIN CAPITAL LETTER SCHWA */</font></i>
<b><font color="#000080">#define</font></b> XK_schwa                      <font color="#993399">0x1000259</font>  <i><font color="#9A1900">/* U+0259 LATIN SMALL LETTER SCHWA */</font></i>
<i><font color="#9A1900">/* those are not really Caucasus */</font></i>
<i><font color="#9A1900">/* For Inupiak */</font></i>
<b><font color="#000080">#define</font></b> XK_Lbelowdot                  <font color="#993399">0x1001e36</font>  <i><font color="#9A1900">/* U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_lbelowdot                  <font color="#993399">0x1001e37</font>  <i><font color="#9A1900">/* U+1E37 LATIN SMALL LETTER L WITH DOT BELOW */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_CAUCASUS */</font></i>

<i><font color="#9A1900">/*</font></i>
<i><font color="#9A1900"> * Vietnamese</font></i>
<i><font color="#9A1900"> */</font></i>
 
<b><font color="#000080">#ifdef</font></b> XK_VIETNAMESE
<b><font color="#000080">#define</font></b> XK_Abelowdot                  <font color="#993399">0x1001ea0</font>  <i><font color="#9A1900">/* U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_abelowdot                  <font color="#993399">0x1001ea1</font>  <i><font color="#9A1900">/* U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Ahook                      <font color="#993399">0x1001ea2</font>  <i><font color="#9A1900">/* U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ahook                      <font color="#993399">0x1001ea3</font>  <i><font color="#9A1900">/* U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Acircumflexacute           <font color="#993399">0x1001ea4</font>  <i><font color="#9A1900">/* U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_acircumflexacute           <font color="#993399">0x1001ea5</font>  <i><font color="#9A1900">/* U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Acircumflexgrave           <font color="#993399">0x1001ea6</font>  <i><font color="#9A1900">/* U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_acircumflexgrave           <font color="#993399">0x1001ea7</font>  <i><font color="#9A1900">/* U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Acircumflexhook            <font color="#993399">0x1001ea8</font>  <i><font color="#9A1900">/* U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_acircumflexhook            <font color="#993399">0x1001ea9</font>  <i><font color="#9A1900">/* U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Acircumflextilde           <font color="#993399">0x1001eaa</font>  <i><font color="#9A1900">/* U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_acircumflextilde           <font color="#993399">0x1001eab</font>  <i><font color="#9A1900">/* U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Acircumflexbelowdot        <font color="#993399">0x1001eac</font>  <i><font color="#9A1900">/* U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_acircumflexbelowdot        <font color="#993399">0x1001ead</font>  <i><font color="#9A1900">/* U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Abreveacute                <font color="#993399">0x1001eae</font>  <i><font color="#9A1900">/* U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_abreveacute                <font color="#993399">0x1001eaf</font>  <i><font color="#9A1900">/* U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Abrevegrave                <font color="#993399">0x1001eb0</font>  <i><font color="#9A1900">/* U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_abrevegrave                <font color="#993399">0x1001eb1</font>  <i><font color="#9A1900">/* U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Abrevehook                 <font color="#993399">0x1001eb2</font>  <i><font color="#9A1900">/* U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_abrevehook                 <font color="#993399">0x1001eb3</font>  <i><font color="#9A1900">/* U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Abrevetilde                <font color="#993399">0x1001eb4</font>  <i><font color="#9A1900">/* U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_abrevetilde                <font color="#993399">0x1001eb5</font>  <i><font color="#9A1900">/* U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Abrevebelowdot             <font color="#993399">0x1001eb6</font>  <i><font color="#9A1900">/* U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_abrevebelowdot             <font color="#993399">0x1001eb7</font>  <i><font color="#9A1900">/* U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Ebelowdot                  <font color="#993399">0x1001eb8</font>  <i><font color="#9A1900">/* U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_ebelowdot                  <font color="#993399">0x1001eb9</font>  <i><font color="#9A1900">/* U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Ehook                      <font color="#993399">0x1001eba</font>  <i><font color="#9A1900">/* U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ehook                      <font color="#993399">0x1001ebb</font>  <i><font color="#9A1900">/* U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Etilde                     <font color="#993399">0x1001ebc</font>  <i><font color="#9A1900">/* U+1EBC LATIN CAPITAL LETTER E WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_etilde                     <font color="#993399">0x1001ebd</font>  <i><font color="#9A1900">/* U+1EBD LATIN SMALL LETTER E WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ecircumflexacute           <font color="#993399">0x1001ebe</font>  <i><font color="#9A1900">/* U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_ecircumflexacute           <font color="#993399">0x1001ebf</font>  <i><font color="#9A1900">/* U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ecircumflexgrave           <font color="#993399">0x1001ec0</font>  <i><font color="#9A1900">/* U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ecircumflexgrave           <font color="#993399">0x1001ec1</font>  <i><font color="#9A1900">/* U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ecircumflexhook            <font color="#993399">0x1001ec2</font>  <i><font color="#9A1900">/* U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ecircumflexhook            <font color="#993399">0x1001ec3</font>  <i><font color="#9A1900">/* U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ecircumflextilde           <font color="#993399">0x1001ec4</font>  <i><font color="#9A1900">/* U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_ecircumflextilde           <font color="#993399">0x1001ec5</font>  <i><font color="#9A1900">/* U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ecircumflexbelowdot        <font color="#993399">0x1001ec6</font>  <i><font color="#9A1900">/* U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_ecircumflexbelowdot        <font color="#993399">0x1001ec7</font>  <i><font color="#9A1900">/* U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Ihook                      <font color="#993399">0x1001ec8</font>  <i><font color="#9A1900">/* U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ihook                      <font color="#993399">0x1001ec9</font>  <i><font color="#9A1900">/* U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ibelowdot                  <font color="#993399">0x1001eca</font>  <i><font color="#9A1900">/* U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_ibelowdot                  <font color="#993399">0x1001ecb</font>  <i><font color="#9A1900">/* U+1ECB LATIN SMALL LETTER I WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Obelowdot                  <font color="#993399">0x1001ecc</font>  <i><font color="#9A1900">/* U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_obelowdot                  <font color="#993399">0x1001ecd</font>  <i><font color="#9A1900">/* U+1ECD LATIN SMALL LETTER O WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Ohook                      <font color="#993399">0x1001ece</font>  <i><font color="#9A1900">/* U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ohook                      <font color="#993399">0x1001ecf</font>  <i><font color="#9A1900">/* U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ocircumflexacute           <font color="#993399">0x1001ed0</font>  <i><font color="#9A1900">/* U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_ocircumflexacute           <font color="#993399">0x1001ed1</font>  <i><font color="#9A1900">/* U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ocircumflexgrave           <font color="#993399">0x1001ed2</font>  <i><font color="#9A1900">/* U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ocircumflexgrave           <font color="#993399">0x1001ed3</font>  <i><font color="#9A1900">/* U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ocircumflexhook            <font color="#993399">0x1001ed4</font>  <i><font color="#9A1900">/* U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ocircumflexhook            <font color="#993399">0x1001ed5</font>  <i><font color="#9A1900">/* U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ocircumflextilde           <font color="#993399">0x1001ed6</font>  <i><font color="#9A1900">/* U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_ocircumflextilde           <font color="#993399">0x1001ed7</font>  <i><font color="#9A1900">/* U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ocircumflexbelowdot        <font color="#993399">0x1001ed8</font>  <i><font color="#9A1900">/* U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_ocircumflexbelowdot        <font color="#993399">0x1001ed9</font>  <i><font color="#9A1900">/* U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Ohornacute                 <font color="#993399">0x1001eda</font>  <i><font color="#9A1900">/* U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_ohornacute                 <font color="#993399">0x1001edb</font>  <i><font color="#9A1900">/* U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ohorngrave                 <font color="#993399">0x1001edc</font>  <i><font color="#9A1900">/* U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ohorngrave                 <font color="#993399">0x1001edd</font>  <i><font color="#9A1900">/* U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ohornhook                  <font color="#993399">0x1001ede</font>  <i><font color="#9A1900">/* U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_ohornhook                  <font color="#993399">0x1001edf</font>  <i><font color="#9A1900">/* U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ohorntilde                 <font color="#993399">0x1001ee0</font>  <i><font color="#9A1900">/* U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_ohorntilde                 <font color="#993399">0x1001ee1</font>  <i><font color="#9A1900">/* U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ohornbelowdot              <font color="#993399">0x1001ee2</font>  <i><font color="#9A1900">/* U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_ohornbelowdot              <font color="#993399">0x1001ee3</font>  <i><font color="#9A1900">/* U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Ubelowdot                  <font color="#993399">0x1001ee4</font>  <i><font color="#9A1900">/* U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_ubelowdot                  <font color="#993399">0x1001ee5</font>  <i><font color="#9A1900">/* U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Uhook                      <font color="#993399">0x1001ee6</font>  <i><font color="#9A1900">/* U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_uhook                      <font color="#993399">0x1001ee7</font>  <i><font color="#9A1900">/* U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Uhornacute                 <font color="#993399">0x1001ee8</font>  <i><font color="#9A1900">/* U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_uhornacute                 <font color="#993399">0x1001ee9</font>  <i><font color="#9A1900">/* U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE */</font></i>
<b><font color="#000080">#define</font></b> XK_Uhorngrave                 <font color="#993399">0x1001eea</font>  <i><font color="#9A1900">/* U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_uhorngrave                 <font color="#993399">0x1001eeb</font>  <i><font color="#9A1900">/* U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Uhornhook                  <font color="#993399">0x1001eec</font>  <i><font color="#9A1900">/* U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_uhornhook                  <font color="#993399">0x1001eed</font>  <i><font color="#9A1900">/* U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Uhorntilde                 <font color="#993399">0x1001eee</font>  <i><font color="#9A1900">/* U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_uhorntilde                 <font color="#993399">0x1001eef</font>  <i><font color="#9A1900">/* U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Uhornbelowdot              <font color="#993399">0x1001ef0</font>  <i><font color="#9A1900">/* U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_uhornbelowdot              <font color="#993399">0x1001ef1</font>  <i><font color="#9A1900">/* U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Ybelowdot                  <font color="#993399">0x1001ef4</font>  <i><font color="#9A1900">/* U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_ybelowdot                  <font color="#993399">0x1001ef5</font>  <i><font color="#9A1900">/* U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW */</font></i>
<b><font color="#000080">#define</font></b> XK_Yhook                      <font color="#993399">0x1001ef6</font>  <i><font color="#9A1900">/* U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_yhook                      <font color="#993399">0x1001ef7</font>  <i><font color="#9A1900">/* U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ytilde                     <font color="#993399">0x1001ef8</font>  <i><font color="#9A1900">/* U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_ytilde                     <font color="#993399">0x1001ef9</font>  <i><font color="#9A1900">/* U+1EF9 LATIN SMALL LETTER Y WITH TILDE */</font></i>
<b><font color="#000080">#define</font></b> XK_Ohorn                      <font color="#993399">0x10001a0</font>  <i><font color="#9A1900">/* U+01A0 LATIN CAPITAL LETTER O WITH HORN */</font></i>
<b><font color="#000080">#define</font></b> XK_ohorn                      <font color="#993399">0x10001a1</font>  <i><font color="#9A1900">/* U+01A1 LATIN SMALL LETTER O WITH HORN */</font></i>
<b><font color="#000080">#define</font></b> XK_Uhorn                      <font color="#993399">0x10001af</font>  <i><font color="#9A1900">/* U+01AF LATIN CAPITAL LETTER U WITH HORN */</font></i>
<b><font color="#000080">#define</font></b> XK_uhorn                      <font color="#993399">0x10001b0</font>  <i><font color="#9A1900">/* U+01B0 LATIN SMALL LETTER U WITH HORN */</font></i>

<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_VIETNAMESE */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_CURRENCY
<b><font color="#000080">#define</font></b> XK_EcuSign                    <font color="#993399">0x10020a0</font>  <i><font color="#9A1900">/* U+20A0 EURO-CURRENCY SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_ColonSign                  <font color="#993399">0x10020a1</font>  <i><font color="#9A1900">/* U+20A1 COLON SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_CruzeiroSign               <font color="#993399">0x10020a2</font>  <i><font color="#9A1900">/* U+20A2 CRUZEIRO SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_FFrancSign                 <font color="#993399">0x10020a3</font>  <i><font color="#9A1900">/* U+20A3 FRENCH FRANC SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_LiraSign                   <font color="#993399">0x10020a4</font>  <i><font color="#9A1900">/* U+20A4 LIRA SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_MillSign                   <font color="#993399">0x10020a5</font>  <i><font color="#9A1900">/* U+20A5 MILL SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_NairaSign                  <font color="#993399">0x10020a6</font>  <i><font color="#9A1900">/* U+20A6 NAIRA SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_PesetaSign                 <font color="#993399">0x10020a7</font>  <i><font color="#9A1900">/* U+20A7 PESETA SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_RupeeSign                  <font color="#993399">0x10020a8</font>  <i><font color="#9A1900">/* U+20A8 RUPEE SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_WonSign                    <font color="#993399">0x10020a9</font>  <i><font color="#9A1900">/* U+20A9 WON SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_NewSheqelSign              <font color="#993399">0x10020aa</font>  <i><font color="#9A1900">/* U+20AA NEW SHEQEL SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_DongSign                   <font color="#993399">0x10020ab</font>  <i><font color="#9A1900">/* U+20AB DONG SIGN */</font></i>
<b><font color="#000080">#define</font></b> XK_EuroSign                      <font color="#993399">0x20ac</font>  <i><font color="#9A1900">/* U+20AC EURO SIGN */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_CURRENCY */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_MATHEMATICAL
<i><font color="#9A1900">/* one, two and three are defined above. */</font></i>
<b><font color="#000080">#define</font></b> XK_zerosuperior               <font color="#993399">0x1002070</font>  <i><font color="#9A1900">/* U+2070 SUPERSCRIPT ZERO */</font></i>
<b><font color="#000080">#define</font></b> XK_foursuperior               <font color="#993399">0x1002074</font>  <i><font color="#9A1900">/* U+2074 SUPERSCRIPT FOUR */</font></i>
<b><font color="#000080">#define</font></b> XK_fivesuperior               <font color="#993399">0x1002075</font>  <i><font color="#9A1900">/* U+2075 SUPERSCRIPT FIVE */</font></i>
<b><font color="#000080">#define</font></b> XK_sixsuperior                <font color="#993399">0x1002076</font>  <i><font color="#9A1900">/* U+2076 SUPERSCRIPT SIX */</font></i>
<b><font color="#000080">#define</font></b> XK_sevensuperior              <font color="#993399">0x1002077</font>  <i><font color="#9A1900">/* U+2077 SUPERSCRIPT SEVEN */</font></i>
<b><font color="#000080">#define</font></b> XK_eightsuperior              <font color="#993399">0x1002078</font>  <i><font color="#9A1900">/* U+2078 SUPERSCRIPT EIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_ninesuperior               <font color="#993399">0x1002079</font>  <i><font color="#9A1900">/* U+2079 SUPERSCRIPT NINE */</font></i>
<b><font color="#000080">#define</font></b> XK_zerosubscript              <font color="#993399">0x1002080</font>  <i><font color="#9A1900">/* U+2080 SUBSCRIPT ZERO */</font></i>
<b><font color="#000080">#define</font></b> XK_onesubscript               <font color="#993399">0x1002081</font>  <i><font color="#9A1900">/* U+2081 SUBSCRIPT ONE */</font></i>
<b><font color="#000080">#define</font></b> XK_twosubscript               <font color="#993399">0x1002082</font>  <i><font color="#9A1900">/* U+2082 SUBSCRIPT TWO */</font></i>
<b><font color="#000080">#define</font></b> XK_threesubscript             <font color="#993399">0x1002083</font>  <i><font color="#9A1900">/* U+2083 SUBSCRIPT THREE */</font></i>
<b><font color="#000080">#define</font></b> XK_foursubscript              <font color="#993399">0x1002084</font>  <i><font color="#9A1900">/* U+2084 SUBSCRIPT FOUR */</font></i>
<b><font color="#000080">#define</font></b> XK_fivesubscript              <font color="#993399">0x1002085</font>  <i><font color="#9A1900">/* U+2085 SUBSCRIPT FIVE */</font></i>
<b><font color="#000080">#define</font></b> XK_sixsubscript               <font color="#993399">0x1002086</font>  <i><font color="#9A1900">/* U+2086 SUBSCRIPT SIX */</font></i>
<b><font color="#000080">#define</font></b> XK_sevensubscript             <font color="#993399">0x1002087</font>  <i><font color="#9A1900">/* U+2087 SUBSCRIPT SEVEN */</font></i>
<b><font color="#000080">#define</font></b> XK_eightsubscript             <font color="#993399">0x1002088</font>  <i><font color="#9A1900">/* U+2088 SUBSCRIPT EIGHT */</font></i>
<b><font color="#000080">#define</font></b> XK_ninesubscript              <font color="#993399">0x1002089</font>  <i><font color="#9A1900">/* U+2089 SUBSCRIPT NINE */</font></i>
<b><font color="#000080">#define</font></b> XK_partdifferential           <font color="#993399">0x1002202</font>  <i><font color="#9A1900">/* U+2202 PARTIAL DIFFERENTIAL */</font></i>
<b><font color="#000080">#define</font></b> XK_emptyset                   <font color="#993399">0x1002205</font>  <i><font color="#9A1900">/* U+2205 NULL SET */</font></i>
<b><font color="#000080">#define</font></b> XK_elementof                  <font color="#993399">0x1002208</font>  <i><font color="#9A1900">/* U+2208 ELEMENT OF */</font></i>
<b><font color="#000080">#define</font></b> XK_notelementof               <font color="#993399">0x1002209</font>  <i><font color="#9A1900">/* U+2209 NOT AN ELEMENT OF */</font></i>
<b><font color="#000080">#define</font></b> XK_containsas                 <font color="#993399">0x100220B</font>  <i><font color="#9A1900">/* U+220B CONTAINS AS MEMBER */</font></i>
<b><font color="#000080">#define</font></b> XK_squareroot                 <font color="#993399">0x100221A</font>  <i><font color="#9A1900">/* U+221A SQUARE ROOT */</font></i>
<b><font color="#000080">#define</font></b> XK_cuberoot                   <font color="#993399">0x100221B</font>  <i><font color="#9A1900">/* U+221B CUBE ROOT */</font></i>
<b><font color="#000080">#define</font></b> XK_fourthroot                 <font color="#993399">0x100221C</font>  <i><font color="#9A1900">/* U+221C FOURTH ROOT */</font></i>
<b><font color="#000080">#define</font></b> XK_dintegral                  <font color="#993399">0x100222C</font>  <i><font color="#9A1900">/* U+222C DOUBLE INTEGRAL */</font></i>
<b><font color="#000080">#define</font></b> XK_tintegral                  <font color="#993399">0x100222D</font>  <i><font color="#9A1900">/* U+222D TRIPLE INTEGRAL */</font></i>
<b><font color="#000080">#define</font></b> XK_because                    <font color="#993399">0x1002235</font>  <i><font color="#9A1900">/* U+2235 BECAUSE */</font></i>
<b><font color="#000080">#define</font></b> XK_approxeq                   <font color="#993399">0x1002248</font>  <i><font color="#9A1900">/* U+2245 ALMOST EQUAL TO */</font></i>
<b><font color="#000080">#define</font></b> XK_notapproxeq                <font color="#993399">0x1002247</font>  <i><font color="#9A1900">/* U+2247 NOT ALMOST EQUAL TO */</font></i>
<b><font color="#000080">#define</font></b> XK_notidentical               <font color="#993399">0x1002262</font>  <i><font color="#9A1900">/* U+2262 NOT IDENTICAL TO */</font></i>
<b><font color="#000080">#define</font></b> XK_stricteq                   <font color="#993399">0x1002263</font>  <i><font color="#9A1900">/* U+2263 STRICTLY EQUIVALENT TO */</font></i>          
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_MATHEMATICAL */</font></i>

<b><font color="#000080">#ifdef</font></b> XK_BRAILLE
<b><font color="#000080">#define</font></b> XK_braille_dot_1                 <font color="#993399">0xfff1</font>
<b><font color="#000080">#define</font></b> XK_braille_dot_2                 <font color="#993399">0xfff2</font>
<b><font color="#000080">#define</font></b> XK_braille_dot_3                 <font color="#993399">0xfff3</font>
<b><font color="#000080">#define</font></b> XK_braille_dot_4                 <font color="#993399">0xfff4</font>
<b><font color="#000080">#define</font></b> XK_braille_dot_5                 <font color="#993399">0xfff5</font>
<b><font color="#000080">#define</font></b> XK_braille_dot_6                 <font color="#993399">0xfff6</font>
<b><font color="#000080">#define</font></b> XK_braille_dot_7                 <font color="#993399">0xfff7</font>
<b><font color="#000080">#define</font></b> XK_braille_dot_8                 <font color="#993399">0xfff8</font>
<b><font color="#000080">#define</font></b> XK_braille_dot_9                 <font color="#993399">0xfff9</font>
<b><font color="#000080">#define</font></b> XK_braille_dot_10                <font color="#993399">0xfffa</font>
<b><font color="#000080">#define</font></b> XK_braille_blank              <font color="#993399">0x1002800</font>  <i><font color="#9A1900">/* U+2800 BRAILLE PATTERN BLANK */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1             <font color="#993399">0x1002801</font>  <i><font color="#9A1900">/* U+2801 BRAILLE PATTERN DOTS-1 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2             <font color="#993399">0x1002802</font>  <i><font color="#9A1900">/* U+2802 BRAILLE PATTERN DOTS-2 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12            <font color="#993399">0x1002803</font>  <i><font color="#9A1900">/* U+2803 BRAILLE PATTERN DOTS-12 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3             <font color="#993399">0x1002804</font>  <i><font color="#9A1900">/* U+2804 BRAILLE PATTERN DOTS-3 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13            <font color="#993399">0x1002805</font>  <i><font color="#9A1900">/* U+2805 BRAILLE PATTERN DOTS-13 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23            <font color="#993399">0x1002806</font>  <i><font color="#9A1900">/* U+2806 BRAILLE PATTERN DOTS-23 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123           <font color="#993399">0x1002807</font>  <i><font color="#9A1900">/* U+2807 BRAILLE PATTERN DOTS-123 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_4             <font color="#993399">0x1002808</font>  <i><font color="#9A1900">/* U+2808 BRAILLE PATTERN DOTS-4 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_14            <font color="#993399">0x1002809</font>  <i><font color="#9A1900">/* U+2809 BRAILLE PATTERN DOTS-14 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_24            <font color="#993399">0x100280a</font>  <i><font color="#9A1900">/* U+280a BRAILLE PATTERN DOTS-24 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_124           <font color="#993399">0x100280b</font>  <i><font color="#9A1900">/* U+280b BRAILLE PATTERN DOTS-124 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_34            <font color="#993399">0x100280c</font>  <i><font color="#9A1900">/* U+280c BRAILLE PATTERN DOTS-34 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_134           <font color="#993399">0x100280d</font>  <i><font color="#9A1900">/* U+280d BRAILLE PATTERN DOTS-134 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_234           <font color="#993399">0x100280e</font>  <i><font color="#9A1900">/* U+280e BRAILLE PATTERN DOTS-234 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1234          <font color="#993399">0x100280f</font>  <i><font color="#9A1900">/* U+280f BRAILLE PATTERN DOTS-1234 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_5             <font color="#993399">0x1002810</font>  <i><font color="#9A1900">/* U+2810 BRAILLE PATTERN DOTS-5 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_15            <font color="#993399">0x1002811</font>  <i><font color="#9A1900">/* U+2811 BRAILLE PATTERN DOTS-15 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_25            <font color="#993399">0x1002812</font>  <i><font color="#9A1900">/* U+2812 BRAILLE PATTERN DOTS-25 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_125           <font color="#993399">0x1002813</font>  <i><font color="#9A1900">/* U+2813 BRAILLE PATTERN DOTS-125 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_35            <font color="#993399">0x1002814</font>  <i><font color="#9A1900">/* U+2814 BRAILLE PATTERN DOTS-35 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_135           <font color="#993399">0x1002815</font>  <i><font color="#9A1900">/* U+2815 BRAILLE PATTERN DOTS-135 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_235           <font color="#993399">0x1002816</font>  <i><font color="#9A1900">/* U+2816 BRAILLE PATTERN DOTS-235 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1235          <font color="#993399">0x1002817</font>  <i><font color="#9A1900">/* U+2817 BRAILLE PATTERN DOTS-1235 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_45            <font color="#993399">0x1002818</font>  <i><font color="#9A1900">/* U+2818 BRAILLE PATTERN DOTS-45 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_145           <font color="#993399">0x1002819</font>  <i><font color="#9A1900">/* U+2819 BRAILLE PATTERN DOTS-145 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_245           <font color="#993399">0x100281a</font>  <i><font color="#9A1900">/* U+281a BRAILLE PATTERN DOTS-245 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1245          <font color="#993399">0x100281b</font>  <i><font color="#9A1900">/* U+281b BRAILLE PATTERN DOTS-1245 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_345           <font color="#993399">0x100281c</font>  <i><font color="#9A1900">/* U+281c BRAILLE PATTERN DOTS-345 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1345          <font color="#993399">0x100281d</font>  <i><font color="#9A1900">/* U+281d BRAILLE PATTERN DOTS-1345 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2345          <font color="#993399">0x100281e</font>  <i><font color="#9A1900">/* U+281e BRAILLE PATTERN DOTS-2345 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12345         <font color="#993399">0x100281f</font>  <i><font color="#9A1900">/* U+281f BRAILLE PATTERN DOTS-12345 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_6             <font color="#993399">0x1002820</font>  <i><font color="#9A1900">/* U+2820 BRAILLE PATTERN DOTS-6 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_16            <font color="#993399">0x1002821</font>  <i><font color="#9A1900">/* U+2821 BRAILLE PATTERN DOTS-16 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_26            <font color="#993399">0x1002822</font>  <i><font color="#9A1900">/* U+2822 BRAILLE PATTERN DOTS-26 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_126           <font color="#993399">0x1002823</font>  <i><font color="#9A1900">/* U+2823 BRAILLE PATTERN DOTS-126 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_36            <font color="#993399">0x1002824</font>  <i><font color="#9A1900">/* U+2824 BRAILLE PATTERN DOTS-36 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_136           <font color="#993399">0x1002825</font>  <i><font color="#9A1900">/* U+2825 BRAILLE PATTERN DOTS-136 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_236           <font color="#993399">0x1002826</font>  <i><font color="#9A1900">/* U+2826 BRAILLE PATTERN DOTS-236 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1236          <font color="#993399">0x1002827</font>  <i><font color="#9A1900">/* U+2827 BRAILLE PATTERN DOTS-1236 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_46            <font color="#993399">0x1002828</font>  <i><font color="#9A1900">/* U+2828 BRAILLE PATTERN DOTS-46 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_146           <font color="#993399">0x1002829</font>  <i><font color="#9A1900">/* U+2829 BRAILLE PATTERN DOTS-146 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_246           <font color="#993399">0x100282a</font>  <i><font color="#9A1900">/* U+282a BRAILLE PATTERN DOTS-246 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1246          <font color="#993399">0x100282b</font>  <i><font color="#9A1900">/* U+282b BRAILLE PATTERN DOTS-1246 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_346           <font color="#993399">0x100282c</font>  <i><font color="#9A1900">/* U+282c BRAILLE PATTERN DOTS-346 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1346          <font color="#993399">0x100282d</font>  <i><font color="#9A1900">/* U+282d BRAILLE PATTERN DOTS-1346 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2346          <font color="#993399">0x100282e</font>  <i><font color="#9A1900">/* U+282e BRAILLE PATTERN DOTS-2346 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12346         <font color="#993399">0x100282f</font>  <i><font color="#9A1900">/* U+282f BRAILLE PATTERN DOTS-12346 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_56            <font color="#993399">0x1002830</font>  <i><font color="#9A1900">/* U+2830 BRAILLE PATTERN DOTS-56 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_156           <font color="#993399">0x1002831</font>  <i><font color="#9A1900">/* U+2831 BRAILLE PATTERN DOTS-156 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_256           <font color="#993399">0x1002832</font>  <i><font color="#9A1900">/* U+2832 BRAILLE PATTERN DOTS-256 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1256          <font color="#993399">0x1002833</font>  <i><font color="#9A1900">/* U+2833 BRAILLE PATTERN DOTS-1256 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_356           <font color="#993399">0x1002834</font>  <i><font color="#9A1900">/* U+2834 BRAILLE PATTERN DOTS-356 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1356          <font color="#993399">0x1002835</font>  <i><font color="#9A1900">/* U+2835 BRAILLE PATTERN DOTS-1356 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2356          <font color="#993399">0x1002836</font>  <i><font color="#9A1900">/* U+2836 BRAILLE PATTERN DOTS-2356 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12356         <font color="#993399">0x1002837</font>  <i><font color="#9A1900">/* U+2837 BRAILLE PATTERN DOTS-12356 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_456           <font color="#993399">0x1002838</font>  <i><font color="#9A1900">/* U+2838 BRAILLE PATTERN DOTS-456 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1456          <font color="#993399">0x1002839</font>  <i><font color="#9A1900">/* U+2839 BRAILLE PATTERN DOTS-1456 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2456          <font color="#993399">0x100283a</font>  <i><font color="#9A1900">/* U+283a BRAILLE PATTERN DOTS-2456 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12456         <font color="#993399">0x100283b</font>  <i><font color="#9A1900">/* U+283b BRAILLE PATTERN DOTS-12456 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3456          <font color="#993399">0x100283c</font>  <i><font color="#9A1900">/* U+283c BRAILLE PATTERN DOTS-3456 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13456         <font color="#993399">0x100283d</font>  <i><font color="#9A1900">/* U+283d BRAILLE PATTERN DOTS-13456 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23456         <font color="#993399">0x100283e</font>  <i><font color="#9A1900">/* U+283e BRAILLE PATTERN DOTS-23456 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123456        <font color="#993399">0x100283f</font>  <i><font color="#9A1900">/* U+283f BRAILLE PATTERN DOTS-123456 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_7             <font color="#993399">0x1002840</font>  <i><font color="#9A1900">/* U+2840 BRAILLE PATTERN DOTS-7 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_17            <font color="#993399">0x1002841</font>  <i><font color="#9A1900">/* U+2841 BRAILLE PATTERN DOTS-17 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_27            <font color="#993399">0x1002842</font>  <i><font color="#9A1900">/* U+2842 BRAILLE PATTERN DOTS-27 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_127           <font color="#993399">0x1002843</font>  <i><font color="#9A1900">/* U+2843 BRAILLE PATTERN DOTS-127 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_37            <font color="#993399">0x1002844</font>  <i><font color="#9A1900">/* U+2844 BRAILLE PATTERN DOTS-37 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_137           <font color="#993399">0x1002845</font>  <i><font color="#9A1900">/* U+2845 BRAILLE PATTERN DOTS-137 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_237           <font color="#993399">0x1002846</font>  <i><font color="#9A1900">/* U+2846 BRAILLE PATTERN DOTS-237 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1237          <font color="#993399">0x1002847</font>  <i><font color="#9A1900">/* U+2847 BRAILLE PATTERN DOTS-1237 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_47            <font color="#993399">0x1002848</font>  <i><font color="#9A1900">/* U+2848 BRAILLE PATTERN DOTS-47 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_147           <font color="#993399">0x1002849</font>  <i><font color="#9A1900">/* U+2849 BRAILLE PATTERN DOTS-147 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_247           <font color="#993399">0x100284a</font>  <i><font color="#9A1900">/* U+284a BRAILLE PATTERN DOTS-247 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1247          <font color="#993399">0x100284b</font>  <i><font color="#9A1900">/* U+284b BRAILLE PATTERN DOTS-1247 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_347           <font color="#993399">0x100284c</font>  <i><font color="#9A1900">/* U+284c BRAILLE PATTERN DOTS-347 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1347          <font color="#993399">0x100284d</font>  <i><font color="#9A1900">/* U+284d BRAILLE PATTERN DOTS-1347 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2347          <font color="#993399">0x100284e</font>  <i><font color="#9A1900">/* U+284e BRAILLE PATTERN DOTS-2347 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12347         <font color="#993399">0x100284f</font>  <i><font color="#9A1900">/* U+284f BRAILLE PATTERN DOTS-12347 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_57            <font color="#993399">0x1002850</font>  <i><font color="#9A1900">/* U+2850 BRAILLE PATTERN DOTS-57 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_157           <font color="#993399">0x1002851</font>  <i><font color="#9A1900">/* U+2851 BRAILLE PATTERN DOTS-157 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_257           <font color="#993399">0x1002852</font>  <i><font color="#9A1900">/* U+2852 BRAILLE PATTERN DOTS-257 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1257          <font color="#993399">0x1002853</font>  <i><font color="#9A1900">/* U+2853 BRAILLE PATTERN DOTS-1257 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_357           <font color="#993399">0x1002854</font>  <i><font color="#9A1900">/* U+2854 BRAILLE PATTERN DOTS-357 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1357          <font color="#993399">0x1002855</font>  <i><font color="#9A1900">/* U+2855 BRAILLE PATTERN DOTS-1357 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2357          <font color="#993399">0x1002856</font>  <i><font color="#9A1900">/* U+2856 BRAILLE PATTERN DOTS-2357 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12357         <font color="#993399">0x1002857</font>  <i><font color="#9A1900">/* U+2857 BRAILLE PATTERN DOTS-12357 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_457           <font color="#993399">0x1002858</font>  <i><font color="#9A1900">/* U+2858 BRAILLE PATTERN DOTS-457 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1457          <font color="#993399">0x1002859</font>  <i><font color="#9A1900">/* U+2859 BRAILLE PATTERN DOTS-1457 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2457          <font color="#993399">0x100285a</font>  <i><font color="#9A1900">/* U+285a BRAILLE PATTERN DOTS-2457 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12457         <font color="#993399">0x100285b</font>  <i><font color="#9A1900">/* U+285b BRAILLE PATTERN DOTS-12457 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3457          <font color="#993399">0x100285c</font>  <i><font color="#9A1900">/* U+285c BRAILLE PATTERN DOTS-3457 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13457         <font color="#993399">0x100285d</font>  <i><font color="#9A1900">/* U+285d BRAILLE PATTERN DOTS-13457 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23457         <font color="#993399">0x100285e</font>  <i><font color="#9A1900">/* U+285e BRAILLE PATTERN DOTS-23457 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123457        <font color="#993399">0x100285f</font>  <i><font color="#9A1900">/* U+285f BRAILLE PATTERN DOTS-123457 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_67            <font color="#993399">0x1002860</font>  <i><font color="#9A1900">/* U+2860 BRAILLE PATTERN DOTS-67 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_167           <font color="#993399">0x1002861</font>  <i><font color="#9A1900">/* U+2861 BRAILLE PATTERN DOTS-167 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_267           <font color="#993399">0x1002862</font>  <i><font color="#9A1900">/* U+2862 BRAILLE PATTERN DOTS-267 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1267          <font color="#993399">0x1002863</font>  <i><font color="#9A1900">/* U+2863 BRAILLE PATTERN DOTS-1267 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_367           <font color="#993399">0x1002864</font>  <i><font color="#9A1900">/* U+2864 BRAILLE PATTERN DOTS-367 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1367          <font color="#993399">0x1002865</font>  <i><font color="#9A1900">/* U+2865 BRAILLE PATTERN DOTS-1367 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2367          <font color="#993399">0x1002866</font>  <i><font color="#9A1900">/* U+2866 BRAILLE PATTERN DOTS-2367 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12367         <font color="#993399">0x1002867</font>  <i><font color="#9A1900">/* U+2867 BRAILLE PATTERN DOTS-12367 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_467           <font color="#993399">0x1002868</font>  <i><font color="#9A1900">/* U+2868 BRAILLE PATTERN DOTS-467 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1467          <font color="#993399">0x1002869</font>  <i><font color="#9A1900">/* U+2869 BRAILLE PATTERN DOTS-1467 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2467          <font color="#993399">0x100286a</font>  <i><font color="#9A1900">/* U+286a BRAILLE PATTERN DOTS-2467 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12467         <font color="#993399">0x100286b</font>  <i><font color="#9A1900">/* U+286b BRAILLE PATTERN DOTS-12467 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3467          <font color="#993399">0x100286c</font>  <i><font color="#9A1900">/* U+286c BRAILLE PATTERN DOTS-3467 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13467         <font color="#993399">0x100286d</font>  <i><font color="#9A1900">/* U+286d BRAILLE PATTERN DOTS-13467 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23467         <font color="#993399">0x100286e</font>  <i><font color="#9A1900">/* U+286e BRAILLE PATTERN DOTS-23467 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123467        <font color="#993399">0x100286f</font>  <i><font color="#9A1900">/* U+286f BRAILLE PATTERN DOTS-123467 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_567           <font color="#993399">0x1002870</font>  <i><font color="#9A1900">/* U+2870 BRAILLE PATTERN DOTS-567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1567          <font color="#993399">0x1002871</font>  <i><font color="#9A1900">/* U+2871 BRAILLE PATTERN DOTS-1567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2567          <font color="#993399">0x1002872</font>  <i><font color="#9A1900">/* U+2872 BRAILLE PATTERN DOTS-2567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12567         <font color="#993399">0x1002873</font>  <i><font color="#9A1900">/* U+2873 BRAILLE PATTERN DOTS-12567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3567          <font color="#993399">0x1002874</font>  <i><font color="#9A1900">/* U+2874 BRAILLE PATTERN DOTS-3567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13567         <font color="#993399">0x1002875</font>  <i><font color="#9A1900">/* U+2875 BRAILLE PATTERN DOTS-13567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23567         <font color="#993399">0x1002876</font>  <i><font color="#9A1900">/* U+2876 BRAILLE PATTERN DOTS-23567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123567        <font color="#993399">0x1002877</font>  <i><font color="#9A1900">/* U+2877 BRAILLE PATTERN DOTS-123567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_4567          <font color="#993399">0x1002878</font>  <i><font color="#9A1900">/* U+2878 BRAILLE PATTERN DOTS-4567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_14567         <font color="#993399">0x1002879</font>  <i><font color="#9A1900">/* U+2879 BRAILLE PATTERN DOTS-14567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_24567         <font color="#993399">0x100287a</font>  <i><font color="#9A1900">/* U+287a BRAILLE PATTERN DOTS-24567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_124567        <font color="#993399">0x100287b</font>  <i><font color="#9A1900">/* U+287b BRAILLE PATTERN DOTS-124567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_34567         <font color="#993399">0x100287c</font>  <i><font color="#9A1900">/* U+287c BRAILLE PATTERN DOTS-34567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_134567        <font color="#993399">0x100287d</font>  <i><font color="#9A1900">/* U+287d BRAILLE PATTERN DOTS-134567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_234567        <font color="#993399">0x100287e</font>  <i><font color="#9A1900">/* U+287e BRAILLE PATTERN DOTS-234567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1234567       <font color="#993399">0x100287f</font>  <i><font color="#9A1900">/* U+287f BRAILLE PATTERN DOTS-1234567 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_8             <font color="#993399">0x1002880</font>  <i><font color="#9A1900">/* U+2880 BRAILLE PATTERN DOTS-8 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_18            <font color="#993399">0x1002881</font>  <i><font color="#9A1900">/* U+2881 BRAILLE PATTERN DOTS-18 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_28            <font color="#993399">0x1002882</font>  <i><font color="#9A1900">/* U+2882 BRAILLE PATTERN DOTS-28 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_128           <font color="#993399">0x1002883</font>  <i><font color="#9A1900">/* U+2883 BRAILLE PATTERN DOTS-128 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_38            <font color="#993399">0x1002884</font>  <i><font color="#9A1900">/* U+2884 BRAILLE PATTERN DOTS-38 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_138           <font color="#993399">0x1002885</font>  <i><font color="#9A1900">/* U+2885 BRAILLE PATTERN DOTS-138 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_238           <font color="#993399">0x1002886</font>  <i><font color="#9A1900">/* U+2886 BRAILLE PATTERN DOTS-238 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1238          <font color="#993399">0x1002887</font>  <i><font color="#9A1900">/* U+2887 BRAILLE PATTERN DOTS-1238 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_48            <font color="#993399">0x1002888</font>  <i><font color="#9A1900">/* U+2888 BRAILLE PATTERN DOTS-48 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_148           <font color="#993399">0x1002889</font>  <i><font color="#9A1900">/* U+2889 BRAILLE PATTERN DOTS-148 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_248           <font color="#993399">0x100288a</font>  <i><font color="#9A1900">/* U+288a BRAILLE PATTERN DOTS-248 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1248          <font color="#993399">0x100288b</font>  <i><font color="#9A1900">/* U+288b BRAILLE PATTERN DOTS-1248 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_348           <font color="#993399">0x100288c</font>  <i><font color="#9A1900">/* U+288c BRAILLE PATTERN DOTS-348 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1348          <font color="#993399">0x100288d</font>  <i><font color="#9A1900">/* U+288d BRAILLE PATTERN DOTS-1348 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2348          <font color="#993399">0x100288e</font>  <i><font color="#9A1900">/* U+288e BRAILLE PATTERN DOTS-2348 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12348         <font color="#993399">0x100288f</font>  <i><font color="#9A1900">/* U+288f BRAILLE PATTERN DOTS-12348 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_58            <font color="#993399">0x1002890</font>  <i><font color="#9A1900">/* U+2890 BRAILLE PATTERN DOTS-58 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_158           <font color="#993399">0x1002891</font>  <i><font color="#9A1900">/* U+2891 BRAILLE PATTERN DOTS-158 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_258           <font color="#993399">0x1002892</font>  <i><font color="#9A1900">/* U+2892 BRAILLE PATTERN DOTS-258 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1258          <font color="#993399">0x1002893</font>  <i><font color="#9A1900">/* U+2893 BRAILLE PATTERN DOTS-1258 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_358           <font color="#993399">0x1002894</font>  <i><font color="#9A1900">/* U+2894 BRAILLE PATTERN DOTS-358 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1358          <font color="#993399">0x1002895</font>  <i><font color="#9A1900">/* U+2895 BRAILLE PATTERN DOTS-1358 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2358          <font color="#993399">0x1002896</font>  <i><font color="#9A1900">/* U+2896 BRAILLE PATTERN DOTS-2358 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12358         <font color="#993399">0x1002897</font>  <i><font color="#9A1900">/* U+2897 BRAILLE PATTERN DOTS-12358 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_458           <font color="#993399">0x1002898</font>  <i><font color="#9A1900">/* U+2898 BRAILLE PATTERN DOTS-458 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1458          <font color="#993399">0x1002899</font>  <i><font color="#9A1900">/* U+2899 BRAILLE PATTERN DOTS-1458 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2458          <font color="#993399">0x100289a</font>  <i><font color="#9A1900">/* U+289a BRAILLE PATTERN DOTS-2458 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12458         <font color="#993399">0x100289b</font>  <i><font color="#9A1900">/* U+289b BRAILLE PATTERN DOTS-12458 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3458          <font color="#993399">0x100289c</font>  <i><font color="#9A1900">/* U+289c BRAILLE PATTERN DOTS-3458 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13458         <font color="#993399">0x100289d</font>  <i><font color="#9A1900">/* U+289d BRAILLE PATTERN DOTS-13458 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23458         <font color="#993399">0x100289e</font>  <i><font color="#9A1900">/* U+289e BRAILLE PATTERN DOTS-23458 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123458        <font color="#993399">0x100289f</font>  <i><font color="#9A1900">/* U+289f BRAILLE PATTERN DOTS-123458 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_68            <font color="#993399">0x10028a0</font>  <i><font color="#9A1900">/* U+28a0 BRAILLE PATTERN DOTS-68 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_168           <font color="#993399">0x10028a1</font>  <i><font color="#9A1900">/* U+28a1 BRAILLE PATTERN DOTS-168 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_268           <font color="#993399">0x10028a2</font>  <i><font color="#9A1900">/* U+28a2 BRAILLE PATTERN DOTS-268 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1268          <font color="#993399">0x10028a3</font>  <i><font color="#9A1900">/* U+28a3 BRAILLE PATTERN DOTS-1268 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_368           <font color="#993399">0x10028a4</font>  <i><font color="#9A1900">/* U+28a4 BRAILLE PATTERN DOTS-368 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1368          <font color="#993399">0x10028a5</font>  <i><font color="#9A1900">/* U+28a5 BRAILLE PATTERN DOTS-1368 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2368          <font color="#993399">0x10028a6</font>  <i><font color="#9A1900">/* U+28a6 BRAILLE PATTERN DOTS-2368 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12368         <font color="#993399">0x10028a7</font>  <i><font color="#9A1900">/* U+28a7 BRAILLE PATTERN DOTS-12368 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_468           <font color="#993399">0x10028a8</font>  <i><font color="#9A1900">/* U+28a8 BRAILLE PATTERN DOTS-468 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1468          <font color="#993399">0x10028a9</font>  <i><font color="#9A1900">/* U+28a9 BRAILLE PATTERN DOTS-1468 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2468          <font color="#993399">0x10028aa</font>  <i><font color="#9A1900">/* U+28aa BRAILLE PATTERN DOTS-2468 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12468         <font color="#993399">0x10028ab</font>  <i><font color="#9A1900">/* U+28ab BRAILLE PATTERN DOTS-12468 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3468          <font color="#993399">0x10028ac</font>  <i><font color="#9A1900">/* U+28ac BRAILLE PATTERN DOTS-3468 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13468         <font color="#993399">0x10028ad</font>  <i><font color="#9A1900">/* U+28ad BRAILLE PATTERN DOTS-13468 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23468         <font color="#993399">0x10028ae</font>  <i><font color="#9A1900">/* U+28ae BRAILLE PATTERN DOTS-23468 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123468        <font color="#993399">0x10028af</font>  <i><font color="#9A1900">/* U+28af BRAILLE PATTERN DOTS-123468 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_568           <font color="#993399">0x10028b0</font>  <i><font color="#9A1900">/* U+28b0 BRAILLE PATTERN DOTS-568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1568          <font color="#993399">0x10028b1</font>  <i><font color="#9A1900">/* U+28b1 BRAILLE PATTERN DOTS-1568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2568          <font color="#993399">0x10028b2</font>  <i><font color="#9A1900">/* U+28b2 BRAILLE PATTERN DOTS-2568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12568         <font color="#993399">0x10028b3</font>  <i><font color="#9A1900">/* U+28b3 BRAILLE PATTERN DOTS-12568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3568          <font color="#993399">0x10028b4</font>  <i><font color="#9A1900">/* U+28b4 BRAILLE PATTERN DOTS-3568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13568         <font color="#993399">0x10028b5</font>  <i><font color="#9A1900">/* U+28b5 BRAILLE PATTERN DOTS-13568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23568         <font color="#993399">0x10028b6</font>  <i><font color="#9A1900">/* U+28b6 BRAILLE PATTERN DOTS-23568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123568        <font color="#993399">0x10028b7</font>  <i><font color="#9A1900">/* U+28b7 BRAILLE PATTERN DOTS-123568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_4568          <font color="#993399">0x10028b8</font>  <i><font color="#9A1900">/* U+28b8 BRAILLE PATTERN DOTS-4568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_14568         <font color="#993399">0x10028b9</font>  <i><font color="#9A1900">/* U+28b9 BRAILLE PATTERN DOTS-14568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_24568         <font color="#993399">0x10028ba</font>  <i><font color="#9A1900">/* U+28ba BRAILLE PATTERN DOTS-24568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_124568        <font color="#993399">0x10028bb</font>  <i><font color="#9A1900">/* U+28bb BRAILLE PATTERN DOTS-124568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_34568         <font color="#993399">0x10028bc</font>  <i><font color="#9A1900">/* U+28bc BRAILLE PATTERN DOTS-34568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_134568        <font color="#993399">0x10028bd</font>  <i><font color="#9A1900">/* U+28bd BRAILLE PATTERN DOTS-134568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_234568        <font color="#993399">0x10028be</font>  <i><font color="#9A1900">/* U+28be BRAILLE PATTERN DOTS-234568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1234568       <font color="#993399">0x10028bf</font>  <i><font color="#9A1900">/* U+28bf BRAILLE PATTERN DOTS-1234568 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_78            <font color="#993399">0x10028c0</font>  <i><font color="#9A1900">/* U+28c0 BRAILLE PATTERN DOTS-78 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_178           <font color="#993399">0x10028c1</font>  <i><font color="#9A1900">/* U+28c1 BRAILLE PATTERN DOTS-178 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_278           <font color="#993399">0x10028c2</font>  <i><font color="#9A1900">/* U+28c2 BRAILLE PATTERN DOTS-278 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1278          <font color="#993399">0x10028c3</font>  <i><font color="#9A1900">/* U+28c3 BRAILLE PATTERN DOTS-1278 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_378           <font color="#993399">0x10028c4</font>  <i><font color="#9A1900">/* U+28c4 BRAILLE PATTERN DOTS-378 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1378          <font color="#993399">0x10028c5</font>  <i><font color="#9A1900">/* U+28c5 BRAILLE PATTERN DOTS-1378 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2378          <font color="#993399">0x10028c6</font>  <i><font color="#9A1900">/* U+28c6 BRAILLE PATTERN DOTS-2378 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12378         <font color="#993399">0x10028c7</font>  <i><font color="#9A1900">/* U+28c7 BRAILLE PATTERN DOTS-12378 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_478           <font color="#993399">0x10028c8</font>  <i><font color="#9A1900">/* U+28c8 BRAILLE PATTERN DOTS-478 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1478          <font color="#993399">0x10028c9</font>  <i><font color="#9A1900">/* U+28c9 BRAILLE PATTERN DOTS-1478 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2478          <font color="#993399">0x10028ca</font>  <i><font color="#9A1900">/* U+28ca BRAILLE PATTERN DOTS-2478 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12478         <font color="#993399">0x10028cb</font>  <i><font color="#9A1900">/* U+28cb BRAILLE PATTERN DOTS-12478 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3478          <font color="#993399">0x10028cc</font>  <i><font color="#9A1900">/* U+28cc BRAILLE PATTERN DOTS-3478 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13478         <font color="#993399">0x10028cd</font>  <i><font color="#9A1900">/* U+28cd BRAILLE PATTERN DOTS-13478 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23478         <font color="#993399">0x10028ce</font>  <i><font color="#9A1900">/* U+28ce BRAILLE PATTERN DOTS-23478 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123478        <font color="#993399">0x10028cf</font>  <i><font color="#9A1900">/* U+28cf BRAILLE PATTERN DOTS-123478 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_578           <font color="#993399">0x10028d0</font>  <i><font color="#9A1900">/* U+28d0 BRAILLE PATTERN DOTS-578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1578          <font color="#993399">0x10028d1</font>  <i><font color="#9A1900">/* U+28d1 BRAILLE PATTERN DOTS-1578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2578          <font color="#993399">0x10028d2</font>  <i><font color="#9A1900">/* U+28d2 BRAILLE PATTERN DOTS-2578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12578         <font color="#993399">0x10028d3</font>  <i><font color="#9A1900">/* U+28d3 BRAILLE PATTERN DOTS-12578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3578          <font color="#993399">0x10028d4</font>  <i><font color="#9A1900">/* U+28d4 BRAILLE PATTERN DOTS-3578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13578         <font color="#993399">0x10028d5</font>  <i><font color="#9A1900">/* U+28d5 BRAILLE PATTERN DOTS-13578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23578         <font color="#993399">0x10028d6</font>  <i><font color="#9A1900">/* U+28d6 BRAILLE PATTERN DOTS-23578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123578        <font color="#993399">0x10028d7</font>  <i><font color="#9A1900">/* U+28d7 BRAILLE PATTERN DOTS-123578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_4578          <font color="#993399">0x10028d8</font>  <i><font color="#9A1900">/* U+28d8 BRAILLE PATTERN DOTS-4578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_14578         <font color="#993399">0x10028d9</font>  <i><font color="#9A1900">/* U+28d9 BRAILLE PATTERN DOTS-14578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_24578         <font color="#993399">0x10028da</font>  <i><font color="#9A1900">/* U+28da BRAILLE PATTERN DOTS-24578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_124578        <font color="#993399">0x10028db</font>  <i><font color="#9A1900">/* U+28db BRAILLE PATTERN DOTS-124578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_34578         <font color="#993399">0x10028dc</font>  <i><font color="#9A1900">/* U+28dc BRAILLE PATTERN DOTS-34578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_134578        <font color="#993399">0x10028dd</font>  <i><font color="#9A1900">/* U+28dd BRAILLE PATTERN DOTS-134578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_234578        <font color="#993399">0x10028de</font>  <i><font color="#9A1900">/* U+28de BRAILLE PATTERN DOTS-234578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1234578       <font color="#993399">0x10028df</font>  <i><font color="#9A1900">/* U+28df BRAILLE PATTERN DOTS-1234578 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_678           <font color="#993399">0x10028e0</font>  <i><font color="#9A1900">/* U+28e0 BRAILLE PATTERN DOTS-678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1678          <font color="#993399">0x10028e1</font>  <i><font color="#9A1900">/* U+28e1 BRAILLE PATTERN DOTS-1678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2678          <font color="#993399">0x10028e2</font>  <i><font color="#9A1900">/* U+28e2 BRAILLE PATTERN DOTS-2678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12678         <font color="#993399">0x10028e3</font>  <i><font color="#9A1900">/* U+28e3 BRAILLE PATTERN DOTS-12678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_3678          <font color="#993399">0x10028e4</font>  <i><font color="#9A1900">/* U+28e4 BRAILLE PATTERN DOTS-3678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_13678         <font color="#993399">0x10028e5</font>  <i><font color="#9A1900">/* U+28e5 BRAILLE PATTERN DOTS-13678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_23678         <font color="#993399">0x10028e6</font>  <i><font color="#9A1900">/* U+28e6 BRAILLE PATTERN DOTS-23678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_123678        <font color="#993399">0x10028e7</font>  <i><font color="#9A1900">/* U+28e7 BRAILLE PATTERN DOTS-123678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_4678          <font color="#993399">0x10028e8</font>  <i><font color="#9A1900">/* U+28e8 BRAILLE PATTERN DOTS-4678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_14678         <font color="#993399">0x10028e9</font>  <i><font color="#9A1900">/* U+28e9 BRAILLE PATTERN DOTS-14678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_24678         <font color="#993399">0x10028ea</font>  <i><font color="#9A1900">/* U+28ea BRAILLE PATTERN DOTS-24678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_124678        <font color="#993399">0x10028eb</font>  <i><font color="#9A1900">/* U+28eb BRAILLE PATTERN DOTS-124678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_34678         <font color="#993399">0x10028ec</font>  <i><font color="#9A1900">/* U+28ec BRAILLE PATTERN DOTS-34678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_134678        <font color="#993399">0x10028ed</font>  <i><font color="#9A1900">/* U+28ed BRAILLE PATTERN DOTS-134678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_234678        <font color="#993399">0x10028ee</font>  <i><font color="#9A1900">/* U+28ee BRAILLE PATTERN DOTS-234678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1234678       <font color="#993399">0x10028ef</font>  <i><font color="#9A1900">/* U+28ef BRAILLE PATTERN DOTS-1234678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_5678          <font color="#993399">0x10028f0</font>  <i><font color="#9A1900">/* U+28f0 BRAILLE PATTERN DOTS-5678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_15678         <font color="#993399">0x10028f1</font>  <i><font color="#9A1900">/* U+28f1 BRAILLE PATTERN DOTS-15678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_25678         <font color="#993399">0x10028f2</font>  <i><font color="#9A1900">/* U+28f2 BRAILLE PATTERN DOTS-25678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_125678        <font color="#993399">0x10028f3</font>  <i><font color="#9A1900">/* U+28f3 BRAILLE PATTERN DOTS-125678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_35678         <font color="#993399">0x10028f4</font>  <i><font color="#9A1900">/* U+28f4 BRAILLE PATTERN DOTS-35678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_135678        <font color="#993399">0x10028f5</font>  <i><font color="#9A1900">/* U+28f5 BRAILLE PATTERN DOTS-135678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_235678        <font color="#993399">0x10028f6</font>  <i><font color="#9A1900">/* U+28f6 BRAILLE PATTERN DOTS-235678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1235678       <font color="#993399">0x10028f7</font>  <i><font color="#9A1900">/* U+28f7 BRAILLE PATTERN DOTS-1235678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_45678         <font color="#993399">0x10028f8</font>  <i><font color="#9A1900">/* U+28f8 BRAILLE PATTERN DOTS-45678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_145678        <font color="#993399">0x10028f9</font>  <i><font color="#9A1900">/* U+28f9 BRAILLE PATTERN DOTS-145678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_245678        <font color="#993399">0x10028fa</font>  <i><font color="#9A1900">/* U+28fa BRAILLE PATTERN DOTS-245678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1245678       <font color="#993399">0x10028fb</font>  <i><font color="#9A1900">/* U+28fb BRAILLE PATTERN DOTS-1245678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_345678        <font color="#993399">0x10028fc</font>  <i><font color="#9A1900">/* U+28fc BRAILLE PATTERN DOTS-345678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_1345678       <font color="#993399">0x10028fd</font>  <i><font color="#9A1900">/* U+28fd BRAILLE PATTERN DOTS-1345678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_2345678       <font color="#993399">0x10028fe</font>  <i><font color="#9A1900">/* U+28fe BRAILLE PATTERN DOTS-2345678 */</font></i>
<b><font color="#000080">#define</font></b> XK_braille_dots_12345678      <font color="#993399">0x10028ff</font>  <i><font color="#9A1900">/* U+28ff BRAILLE PATTERN DOTS-12345678 */</font></i>
<b><font color="#000080">#endif</font></b> <i><font color="#9A1900">/* XK_BRAILLE */</font></i>
</tt></pre>
