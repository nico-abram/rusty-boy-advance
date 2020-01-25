# Rusty Boy Advance

[![Build Status](https://travis-ci.com/nico-abram/rusty-boy-advance.svg?branch=master)](https://travis-ci.com/nico-abram/rusty-boy-advance)

**_[docs](https://nico-abram.github.io/rusty-boy-advance/doc/rusty_boy_advance/)_**

An attempt at a Game Boy Advance emulator in rust (Very much a WIP).

# Progress

I've only been working on the cpu so far. Almost all instructions are implemented (Except the coprocessor instructions, which aren't used in the GBA), sans bugs/errors. It can execute about 590500 (Not the first N instructions of the code, I mean executed instructions, so a loop would execute the same ones many times) instructions of the bios startup code correctly before going awry (Comparing the contents of all registers against mgba's after every instruction executed) and run the armwrestler test rom (Although it doesn't pass all the tests). I've also worked a bit on an ImGui front end that has a little memory viewer, a cpu state viewer and lets one set breakpoints at a given instruction (Although it doesn't have a disassembler, at least yet) and a web one using the quicksilver crate available at https://nico-abram.github.io/rusty-boy-advance/.

# Useful references/tools

- [GBATEK](http://problemkaputt.de/gbatek.htm)
- The [arm7TDMI manual](http://infocenter.arm.com/help/topic/com.arm.doc.ddi0210c/DDI0210B.pdf)
- The [emulation development discord](https://discord.gg/26wfbS4)
- [This](https://onlinedisassembler.com/odaweb/) online disassembler that supports armv4t
- [This](http://shell-storm.org/online/Online-Assembler-and-Disassembler/) other online disassembler, which works better (The other one doesnt correctly respect the thumb setting all the time) and also has an assembler, but gets some opcodes wrong (Possibly because it's not specifically armv4)
- [This](https://hexed.it/) online hex editor
- [This](https://github.com/Emu-Docs/Emu-Docs/blob/master/Game%20Boy%20Advance/test_roms/arm_wrestler/armwrestler.gba) cpu test rom.
- [The MGBA blog posts](https://mgba.io/).
- [Tonc](https://www.coranac.com/tonc/text/toc.htm). This is intended for GBA programmers, but contains useful reading material and examples, which have been useful as small ROMs for tests.
