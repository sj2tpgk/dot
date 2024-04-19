; === Auto Reload
#SingleInstance force
FileGetTime ScriptStartModTime, %A_ScriptFullPath%
SetTimer CheckScriptUpdate, 2000, 0x7FFFFFFF



; === Auto-focus powershell script

Run, powershell  -noLogo -ExecutionPolicy unrestricted -file "%A_ScriptDir%\\bin\my-autofocus\my-autofocus2.ps1",, Hide



; === Modifiers

#InstallKeybdHook
SetCapsLockState, alwaysoff

Hotkey lAlt, xcapeLabel
Hotkey F14,  xcapeLabel  ; 変換キー
Hotkey F13,  xcapeLabel  ; カタカナひらがなキー
Hotkey rAlt, xcapeLabel

; 無変換キー
; SendInput is faster than Send
F15 & j::  SendInput {Left}
F15 & k::  SendInput {Down}
F15 & l::  SendInput {Up}
F15 & `;:: SendInput {Right}

F15 & m:: SendInput {Enter}
F15 & f:: SendInput {Enter}

F15 & o:: SendInput {Delete}
F15 & p:: SendInput {Backspace}

F15 & a:: SendInput {Home}
F15 & e:: SendInput {End}

; F15 Up::
;   ; MsgBox [%A_ThisHotKey%,%A_PriorKey%]
;   Send {Enter}



; === Key mapping
q::q
w::w
e::f
r::d
t::b
y::;
u::j
i::l
o::u
p::o

a::a
s::r
d::s
f::t
g::g
h::y
j::k
k::n
l::e
`;::i

z::x
x::v
c::c
v::p
b::z
n::,
m::m
,::h
vkE2::_

; $ means no recursion
;$+(::Send {[}
;$+)::Send {]}
;F13 & 8::Send {[}
;F13 & 9::Send {]}
;$[::Send {(}
;$]::Send {)}
;F13 & [::Send {{}
;F13 & ]::Send {}}



; === Misc (above modifiers won't work if this section appears earlier than them.)

$F1:: Send #^{Left}  ; prev workspace
$F2:: Send #^{Right} ; next workspace
$#F1::
$#F2::

#d:: WinSet, Style, ^0xC00000, A ; toggle titlebar
#a:: Winset, Alwaysontop, , A    ; toggle always on top

#MaxHotkeysPerInterval 200 ; for fast key repeat



return ; === Definitions of functions and labels will follow



; === Xcape

xcapeLabel:
  xcape()
  return

xcape() {

  xcapeConfig := { "lAlt": ["lAlt", "!{Tab}"], "F14": ["Control", "{Esc}"], "F13": ["Shift", "!1"], "rAlt": ["rAlt", "^b"] }

  from   := A_ThisHotKey ; "F13" ; quotes are needed
  mod    := xcapeConfig[from][1]
  single := xcapeConfig[from][2]
  Send {%mod% Down}
  KeyWait %from%
  Send {%mod% Up}
  ;MsgBox [%A_ThisHotKey%,%A_PriorKey%,%from%,%mod%,%single%]
  if (A_PriorKey = from) {
      Send %single%
      ; vimmer-ahk companion
      if (A_PriorKey = "F14") {
          IME_SET(0)
      }
  }
  return

}



; === vimmer-ahk https://github.com/koirand/vimmer-ahk/blob/master/vimmer-ahk.ahk
; 以下サイトから拝借
; http://www6.atwiki.jp/eamat/pages/17.html
IME_SET(SetSts, WinTitle="A")    {
    ControlGet,hwnd,HWND,,,%WinTitle%
    if  (WinActive(WinTitle))   {
        ptrSize := !A_PtrSize ? 4 : A_PtrSize
        VarSetCapacity(stGTI, cbSize:=4+4+(PtrSize*6)+16, 0)
        NumPut(cbSize, stGTI,  0, "UInt")  ;   DWORD   cbSize;
        hwnd := DllCall("GetGUIThreadInfo", Uint,0, Uint,&stGTI)
                 ? NumGet(stGTI,8+PtrSize,"UInt") : hwnd
    }
    return DllCall("SendMessage"
          , UInt, DllCall("imm32\ImmGetDefaultIMEWnd", Uint,hwnd)
          , UInt, 0x0283 ;Message : WM_IME_CONTROL
          ,  Int, 0x006  ;wParam  : IMC_SETOPENSTATUS
          ,  Int, SetSts) ;lParam  : 0 or 1
}

~Esc::IME_SET(0)
~^[::IME_SET(0)



; === Auto Reload

CheckScriptUpdate() {
    global ScriptStartModTime
    FileGetTime curModTime, %A_ScriptFullPath%
    If (curModTime == ScriptStartModTime)
        return
    SetTimer CheckScriptUpdate, Off
    Loop
    {
        reload
        Sleep 300 ; ms
        MsgBox 0x2, %A_ScriptName%, Reload failed. ; 0x2 = Abort/Retry/Ignore
        IfMsgBox Abort
            ExitApp
        IfMsgBox Ignore
            break
    } ; loops reload on "Retry"
}

; vim: set cms=;%s
