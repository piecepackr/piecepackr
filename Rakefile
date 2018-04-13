desc "Clean"
task :clean do
    dirs = ["svg", "pdf"]
    dirs.each { |dir|
        sh "rm -r #{dir} | true"
    }
    sh "killall evince | true"
end

demos = ["chess", "chinese_zodiac", "crown_and_anchor", "default",
         "dual", "hex", "orthodox", "rainbow_deck", "reversi", "sixpack"]

version_str = " (v" + `Rscript -e 'cat(packageDescription("piecepack")$Version)'` + ")'"

collect_piecepacks = "exec/collect_piecepacks --font='Noto Sans' --author='Trevor L Davis' "
def make_piecepack (filename, extra_flags)
    configure_piecepack = "exec/configure_piecepack --font='Noto Sans Symbols' --header_font='Noto Sans' "
    json_file = "configurations/" + filename + ".json"
    sh configure_piecepack + " --file=" + json_file + " --deck_filename=" + filename + extra_flags
    sh "exec/make_preview --file=" + json_file
    sh "exec/make_piecepack --file=" + json_file
end

# Colors
chinese_elements_colors = " --suit_colors=darkgreen,red3,chocolate4,black,darkblue,grey"
# alt = "white,orange2,yellow,purple"
light = "hotpink2,dimgrey,palegreen,lightblue2,"
dark = "darkred,black,darkgreen,darkblue,"
light_scheme = light + "grey"
# dark_scheme = dark + "grey"
hexlines_dark = " --hexline_colors=" + dark + "transparent"
hexlines_light = " --hexline_colors=" + light + "transparent"
rd_dark = "#ff0000,#ffc000,#008000,#0000ff,#800080,#000000,white"
rd_light = "#ff80c0,#ffff80,#80ff00,#80ffff,#c080ff,#c0c0c0,white"

# Suits
ca_suits = " --suit_symbols=♥,♦,♣,♠,♚,⚓,★ --suit_symbols_scale=1,1,1,1,1,1,1,1 --suit_symbols_font='Noto Sans Symbols'"
jcd_suits = " --suit_symbols=🌞,🌜,♚,⚓,꩜ --suit_symbols_scale=0.7,0.8,1,1,1.2 --suit_symbols_font='Noto Emoji,Noto Sans Symbols2,Noto Sans Symbols,DejaVu Sans,Noto Sans Cham'"
# french_suits = " --suit_symbols=♥,♠,♣,♦,★ --suit_symbols_scale=1,1,1,1,1,1 --suit_symbols_font='Noto Sans Symbols'"
french_suits_swirl = " --suit_symbols=♥,♠,♣,♦,꩜ --suit_symbols_scale=1,1,1,1,1.2,1.2"
# latin_suits_swirl = " --suit_symbols=🏆,🗡️,⚕️,𐇛,꩜ --suit_symbols_scale=0.6,1,1,1.1,1.2"
latin_suits_swirl = " --suit_symbols=,,,,꩜ --suit_symbols_font=Quivira --suit_symbols_scale=1,1,1,1,1.2"
piecepack_suits = " --suit_symbols=🌞,🌜,👑,⚜,꩜ --suit_symbols_scale=0.7,0.8,0.8,1.1,1.2,1.2 --suit_symbols_font='Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham'"
rd_suits = " --suit_symbols=♥,★,♣,♦,♛,♠,꩜ --suit_symbols_scale=1,1,1,1,0.85,1,1.2 --suit_symbols_font='Noto Sans Symbols'"
# swiss_suits = " --suit_symbols=🌹,⛊,🌰,🔔,★ --suit_symbols_scale=0.7,1.0,0.8,0.8,1.2,1.2"
swiss_suits = " --suit_symbols=,,,, --suit_symbols_font=Quivira --suit_symbols_scale=1"
sixpack_suits = " --suit_symbols=♥,♠,♣,♦,🌞,🌜,꩜ --suit_symbols_scale=1,1,1,1,0.8,0.8,1.2,1.2 --suit_symbols_font='Noto Emoji'"
sixpack_suits_black = " --suit_symbols=♥,♠,♣,♦,☀,🌘,꩜ --suit_symbols_scale=1,1,1,1,1.2,0.9,1"
sixpack_suits_white = " --suit_symbols=♡,♤,♧,♢,☼,🌔,꩜ --suit_symbols_scale=1,1,1,1,1.4,0.9,1"
black_suits4 = " --suit_symbols=♠,♣,☽,⚓,★  --suit_symbols_font='Noto Sans Symbols,Noto Sans Symbols,Noto Sans Symbols,DejaVu Sans,Noto Sans Symbols'"
coin_black_suits4 = " --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1 --suit_symbols_font.coin_back='Noto Sans Symbols' --dm_symbols.coin_back=♠,♣,☽,⚓,★  --dm_symbols_font.coin_back='Noto Sans Symbols,Noto Sans Symbols,Noto Sans Symbols2,DejaVu Sans,Noto Sans Symbols'"
black_suits6 = " --suit_symbols=♠,♣,🌜,⚓,,,★  --suit_symbols_font='Noto Sans Symbols,Noto Sans Symbols,Noto Sans Symbols2,DejaVu Sans,Quivira,Quivira,Noto Sans Symbols' --suit_symbols_scale=1,1,0.8,1,1,1,1"
coin_black_suits6 = " --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1 --suit_symbols_font.coin_back='Noto Sans Symbols' --dm_symbols.coin_back=♠,♣,🌜,⚓,,,★  --dm_symbols_font.coin_back='Noto Sans Symbols,Noto Sans Symbols,Noto Sans Symbols2,DejaVu Sans,Quivira,Quivira,Noto Sans Symbols' --dm_symbols_scale.coin_back=1,1,0.8,1,1,1,1"
red_suits4 = " --suit_symbols=♥,♦,☉,♚,★  --suit_symbols_font='Noto Sans Symbols'"
coin_red_suits4 = " --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1 --suit_symbols_font.coin_back='Noto Sans Symbols' --dm_symbols.coin_back=♥,♦,☉,♚,★  --dm_symbols_font.coin_back='Noto Sans Symbols'"
red_suits6 = " --suit_symbols=♥,♦,🌞,♚,,,★  --suit_symbols_font='Noto Sans Symbols,Noto Sans Symbols,Noto Emoji,Noto Sans Symbols,Quivira,Quivira,Noto Sans Symbols' --suit_symbols_scale=1,1,0.8,1,1,1,1"
coin_red_suits6 = " --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1 --suit_symbols_font.coin_back='Noto Sans Symbols' --dm_symbols.coin_back=♥,♦,🌞,♚,,,★  --dm_symbols_font.coin_back='Noto Sans Symbols,Noto Sans Symbols,Noto Emoji,Noto Sans Symbols,Quivira,Quivira,Noto Sans Symbols' --dm_symbols_scale.coin_back=1,1,0.8,1,1,1,1"
alchemical_elements = " --suit_symbols=🜁,🜂,🜃,🜄, --suit_colors=red,khaki2,green,cornflowerblue,grey"
# emoji_elements = " --suit_symbols=🔥,🌪️,⛰️,🌊, --suit_colors=darkred,black,darkgreen,darkblue"
chinese_elements_emoji = " --suit_symbols=🌲,🔥,⛰️,🏆,🌊, --suit_symbols_scale=1.1,1.1,1,0.6,0.6 --suit_symbols_font='Quivira,Quivira,Quivira,Noto Sans Symbols2,Noto Emoji,'" + chinese_elements_colors
chinese_elements_sc = " --suit_symbols=木,火,土,金,水, --suit_symbols_scale=0.7 --suit_symbols_font='Noto Sans CJK SC'" + chinese_elements_colors 

# Ranks
default_ranks = " --rank_symbols=N,A,2,3,4,5 --rank_symbols_font='Noto Sans'"
orthodox_ranks = " --rank_symbols=,A,2,3,4,5 --use_suit_as_ace --rank_symbols_font='Noto Sans'"
# chess_ranks = " --rank_symbols=P,N,B,R,Q,K --rank_symbols_font='Chess Utrecht' --rank_symbols_scale=1.3"
chess_ranks = " --rank_symbols=♟,♞,♝,♜,♛,♚ --rank_symbols_font='Sans Noto Symbols'"
# chess_ranks = " --rank_symbols=♘,♙,♗,♖,♕,♔"
chinese_ranks_emoji1 = " --rank_symbols=🐀,🐉,🐒,🐂,🐍,🐓 --rank_symbols_scale=0.6 --rank_symbols_font='Noto Emoji'"
chinese_ranks_emoji2 = " --rank_symbols=🐅,🐎,🐕,🐇,🐏,🐖 --rank_symbols_scale=0.6 --rank_symbols_font='Noto Emoji'"
chinese_ranks_sc1 = " --rank_symbols=鼠,龙,猴,牛,蛇,鸡 --rank_symbols_font='Noto Sans CJK SC' --rank_symbols_scale=0.7"
chinese_ranks_sc2 = " --rank_symbols=虎,马,狗,兔,羊,猪 --rank_symbols_font='Noto Sans CJK SC' --rank_symbols_scale=0.7"

# Configurations
dozenal_chips = " --shape.chip_face=5 --shape.chip_back=5"
pyramid_configuration = " --rank_symbols.chip_face='A,B,C,D,E,F' --use_ace_as_ace.chip_face --dm_symbols.chip_face= --dm_symbols.chip_back= --shape.chip_face=kite --shape.chip_back=kite"
orthodox_dm = " --dm_colors.coin_face=black --dm_colors.coin_back=black --dm_symbols.piecepack_die= --dm_symbols.coin_face='|' --dm_symbols_font.coin_face='Noto Sans' --dm_r.coin_face=0.45 --dm_symbols.coin_back='|' --dm_symbols_font.coin_back='Noto Sans' --dm_r.coin_back=0.45"
# ○ ⚆ ⚇ ● ⚈ ⚉
orthodox_saucers = " --suit_symbols.saucer_back= --dm_colors.saucer_face=black --dm_symbols.saucer_face='|' --dm_symbols_font.saucer_face='Noto Sans' --dm_r.saucer_face=0.45 --dm_symbols.saucer_back='|' --dm_symbols_font.saucer_back='Noto Sans' --dm_r.saucer_back=0.45"

orthodox_saucers1 = " --suit_symbols.saucer_face=● --suit_symbols_scale.saucer_face=1 --suit_symbols_font.saucer_face='Noto Sans Symbols'" + orthodox_saucers
orthodox_saucers2 = " --suit_symbols.saucer_face=⚈,⚉,⚈,⚉, --suit_symbols_scale.saucer_face=0.7 --suit_symbols_font.saucer_face='Noto Sans Symbols'" + orthodox_saucers
orthodox_saucers3 = " --suit_symbols.saucer_face=⚈,⚈,⚉,⚉,●,●, --suit_symbols_scale.saucer_face=0.7,0.7,0.7,0.7,1,1 --suit_symbols_font.saucer_face='Noto Sans Symbols'" + orthodox_saucers
hex_components = " --shape.tile_face=6 --shape.tile_back=6 --shape.coin_face=3 --shape.coin_back=3 --shape.saucer_face=3 --shape.saucer_back=3 --shape.chip_face=3 --shape.chip_back=3 --dm_symbols_scale.chip_face=0.7 --dm_symbols_scale.chip_back=0.7 --dm_r.coin_face=0.3 --dm_r.coin_back=0.3 --dm_r.saucer_face=0.3 --dm_r.saucer_back=0.3 --dm_r.chip_face=0.3 --dm_r.chip_back=0.3 --shape_theta.coin_face=-90 --shape_theta.coin_back=-90 --shape_theta.saucer_face=-90 --shape_theta.saucer_back=-90 --shape_theta.chip_face=-90 --shape_theta.chip_back=-90 --dm_theta.coin_face=-90 --dm_theta.coin_back=-90 --dm_theta.saucer_face=-90 --dm_theta.saucer_back=-90 --dm_theta.chip_face=-90 --dm_theta.chip_back=-90"
star_chips  = " --shape.chip_face=star --shape.chip_back=star --rank_symbols_scale.chip_face=0.7 --suit_symbols_scale.chip_back=0.7 --dm_symbols_scale.chip_face=0.7 --dm_symbols_scale.chip_back=0.7 --dm_r.chip_face=0.3 --dm_r.chip_back=0.3"

## Demos
desc "Run all demos"
task :all do
    demos.each { |demo|
        Rake::Task["#{demo}"].invoke()
    }
end

desc "Chess demo"
task :chess do
    deck_title = " --deck_title='Piecepack-suits" + version_str
    file = "chess1"
    make_piecepack file, deck_title + piecepack_suits + chess_ranks

    deck_title = " --deck_title='French-suits (checkered tile faces)" + version_str
    file = "chess2"
    extra_flags = " --checker_colors=grey,grey,grey,grey, --suit_colors=darkred,black,dimgrey,hotpink2,grey"
    make_piecepack file, deck_title + french_suits_swirl + chess_ranks + extra_flags 

    deck_title = " --deck_title='Black-suits (checkered tile backs)" + version_str
    file = "chess3"
    extra_flags = " --checker_colors=,,,,darkred --gridline_colors=  --suit_colors=black,black,black,black,darkred" + coin_black_suits4
    make_piecepack file, deck_title + black_suits4 + chess_ranks + extra_flags 

    deck_title = " --deck_title='Red-suits (checkered tile backs)" + version_str
    file = "chess4"
    extra_flags = " --checker_colors=,,,,black --gridline_colors= --suit_colors=darkred,darkred,darkred,darkred,black" + coin_red_suits4
    make_piecepack file, deck_title + red_suits4 + chess_ranks + extra_flags 

    deck_title = " --deck_title='Black-suits (checkered tile faces)" + version_str
    file = "chess5"
    extra_flags = " --checker_colors=grey,grey,grey,grey,grey,grey, --suit_colors=black,black,black,black,black,black,darkred" + coin_black_suits6
    make_piecepack file, deck_title + black_suits6 + chess_ranks + extra_flags 

    deck_title = " --deck_title='Red-suits (checkered tile faces)" + version_str
    file = "chess6"
    extra_flags = " --checker_colors=grey,grey,grey,grey,grey,grey, --suit_colors=darkred,darkred,darkred,darkred,darkred,darkred,black " + coin_red_suits6
    make_piecepack file, deck_title + red_suits6 + chess_ranks + extra_flags 

    decks = " --decks=chess1,chess2,chess3,chess4,chess5,chess6"
    sh collect_piecepacks + " --filename=chess_demo --title='Chess ranked piecepack demo" + version_str + decks

end

desc "Chinese zodiac demo"
task :chinese_zodiac do
    file = "chinese_zodiac1"
    deck_title = " --deck_title='Chinese zodiac #1" + version_str
    make_piecepack file, deck_title + chinese_elements_emoji + chinese_ranks_emoji1 + dozenal_chips

    deck_title = " --deck_title='Chinese zodiac #2" + version_str
    file = "chinese_zodiac2"
    make_piecepack file, deck_title + chinese_elements_emoji + chinese_ranks_emoji2 + dozenal_chips

    deck_title = " --deck_title='Chinese zodiac #3" + version_str
    file = "chinese_zodiac3"
    make_piecepack file, deck_title + chinese_elements_sc + chinese_ranks_sc1 + dozenal_chips

    deck_title = " --deck_title='Chinese zodiac #4" + version_str
    file = "chinese_zodiac4"
    make_piecepack file, deck_title + chinese_elements_sc + chinese_ranks_sc2 + dozenal_chips

    decks = " --decks=chinese_zodiac1,chinese_zodiac2,chinese_zodiac3,chinese_zodiac4"
    sh collect_piecepacks + " --filename=chinese_zodiac_demo --title='Chinese zodiac demo" + version_str + decks
end


desc "Crown and anchor demo"
task :crown_and_anchor do

    deck_title = " --deck_title='Crown and anchor suits" + version_str
    file = "crown_and_anchor1"
    extra_flags = " --suit_colors=darkred,darkred,black,black,darkred,black,grey"
    make_piecepack file,  deck_title + ca_suits + default_ranks + extra_flags 

    deck_title = " --deck_title='Crown and anchor suits (Multicolor)" + version_str
    file = "crown_and_anchor2"
    extra_flags = " --suit_colors=darkred,darkblue,darkgreen,black,purple,orange2,grey"
    make_piecepack file, deck_title + ca_suits + default_ranks + extra_flags

    deck_title = " --deck_title='JCD piecepack suits (Monoscale)" + version_str
    file = "crown_and_anchor3"
    extra_flags = " --suit_colors=black"
    make_piecepack file, deck_title + jcd_suits + default_ranks + extra_flags

    deck_title = " --deck_title='JCD piecepack suits (Black/Red)" + version_str
    file = "crown_and_anchor4"
    extra_flags = " --suit_colors=darkred,black,darkred,black,grey"
    make_piecepack file, deck_title + jcd_suits + default_ranks + extra_flags

    deck_title = " --deck_title='JCD piecepack suits (Multicolor)" + version_str
    file = "crown_and_anchor5"
    extra_flags = " --suit_colors=darkred,black,darkgreen,darkblue,grey"
    make_piecepack file, deck_title + jcd_suits + default_ranks + extra_flags

    deck_title = " --deck_title='JCD piecepack suits (Alternative Multicolor)" + version_str
    file = "crown_and_anchor6"
    extra_flags = " --suit_colors=yellow,white,purple,orange2,tan4 --background_color=seashell3"
    make_piecepack file, deck_title + jcd_suits + default_ranks + extra_flags

    title = " --title='\"Crown and anchor\" demo" + version_str
    decks = " --decks=crown_and_anchor1,crown_and_anchor2,crown_and_anchor3,crown_and_anchor4,crown_and_anchor5,crown_and_anchor6"
    sh collect_piecepacks + " --filename=crown_and_anchor_demo" + title + decks
end

desc "Default piecepack demo"
task :default do
    file = "default"
    extra_flags = " --font='Noto Sans Symbols' --header_font='Noto Sans' --rank_symbols_font='Noto Sans'"
    make_piecepack file, extra_flags

    decks = " --decks=default"
    title = " --title='Default demo" + version_str
    sh collect_piecepacks + " --filename=default_demo" + title + decks
end


desc "Dual piecepacks demo"
task :dual do

    deck_title = " --deck_title='Piecepack-suited (Un-inverted color scheme)" + version_str
    file = "dual1"
    extra_flags = ""
    make_piecepack file, deck_title + piecepack_suits + default_ranks + extra_flags 

    deck_title = " --deck_title='Latin-suited (Inverted color scheme)" + version_str
    file = "dual2"
    extra_flags = "  --background_color=white --invert_colors.suited"
    make_piecepack file, deck_title + latin_suits_swirl + default_ranks + extra_flags

    deck_title = " --deck_title='French-suited (Dark color scheme)" + version_str
    file = "dual3"
    extra_flags = hexlines_dark
    make_piecepack file, deck_title + french_suits_swirl + default_ranks + extra_flags 

    deck_title = " --deck_title='French-suited (Light color scheme)" + version_str
    file = "dual4"
    extra_flags = " --suit_colors=" + light_scheme + hexlines_light
    make_piecepack file, deck_title + french_suits_swirl + default_ranks + extra_flags

    deck_title = " --deck_title='Swiss-suited (Black color scheme)" + version_str
    black_scheme = " --suit_colors=black,black,black,black,grey40 --background_color=grey70"
    file = "dual5"
    extra_flags = " --invert_colors.suited --use_suit_as_ace" + black_scheme
    make_piecepack file, deck_title + swiss_suits + orthodox_ranks + extra_flags

    deck_title = " --deck_title='Swiss-suited (White color scheme)" + version_str
    white_scheme = " --suit_colors=white,white,white,white,grey40 --background_color=grey70"
    file = "dual6"
    extra_flags = " --invert_colors.suited --use_suit_as_ace" + white_scheme
    make_piecepack file, deck_title + swiss_suits + orthodox_ranks + extra_flags

    title = " --title='\"Dual piecepacks\" proof of concept" + version_str
    decks = " --decks=dual1,dual2,dual3,dual4,dual5,dual6"
    sh collect_piecepacks + " --filename=dual_demo" + title + decks
end

desc "Hex-friendly piecepacks demo"
task :hex do
    deck_title = " --deck_title='French-suited (Colored hexlines on tile faces)" + version_str
    file = "hex1"
    extra_flags = hexlines_dark
    make_piecepack file, deck_title + french_suits_swirl + default_ranks + extra_flags 

    deck_title = " --deck_title='French-suited (Grey hexlines on tile faces/backs)" + version_str
    file = "hex2"
    extra_flags = " --hexline_colors=grey"
    make_piecepack file, deck_title + french_suits_swirl + default_ranks + extra_flags 

    deck_title = " --deck_title='Piecepack-suited (Hex-shaped tile faces/backs)" + version_str
    file = "hex3"
    extra_flags = " --dm_theta.tile_face=120 --dm_r.tile_face=0.3" + hex_components
    make_piecepack file, deck_title + french_suits_swirl + default_ranks + extra_flags 

    deck_title = " --deck_title='Piecepack-suited (Hexpack-ish style)" + version_str
    file = "hex4"
    extra_flags = " --shape_theta.tile_face=0 --shape_theta.tile_back=0 --dm_r.tile_face=0.3 --dm_theta.tile_face=-90 --gridline_colors= --checker_colors=,,,,grey --suit_colors.chip_back=" + hex_components
    make_piecepack file, deck_title + french_suits_swirl + default_ranks + extra_flags 

    decks = " --decks=hex1,hex2,hex3,hex4"
    sh collect_piecepacks + " --filename=hex_demo --title='Hex-friendly piecepack demo" + version_str + decks
end

desc "Orthodox piecepacks demo"
task :orthodox do
    deck_title = " --deck_title='Orthodox piecepack" + version_str
    suit_symbols = piecepack_suits + " --suit_colors=darkred,black,darkgreen,darkblue,black"
    file = "orthodox1_piecepack"
    extra_flags = " --use_suit_as_ace" + pyramid_configuration + orthodox_dm + orthodox_saucers1
    make_piecepack file, deck_title + suit_symbols + orthodox_ranks + extra_flags

    deck_title = " --deck_title='Orthodox-style 2-color french-suited piecepack" + version_str
    suit_symbols = french_suits_swirl + " --suit_colors=darkred,black,black,darkred,black "
    file = "orthodox2_french"
    extra_flags = " --use_suit_as_ace" + pyramid_configuration + orthodox_dm + orthodox_saucers2
    make_piecepack file, deck_title + suit_symbols + orthodox_ranks + extra_flags 

    decks = " --decks=orthodox1_piecepack,orthodox2_french"
    sh collect_piecepacks + " --filename=orthodox_demo --title='Orthodox demo" + version_str + decks
end

desc "Rainbow deck demo"
task :rainbow_deck do
    rd_background = " --background_color=sienna"
    deck_title = " --deck_title='RD-suited piecepack (Dark color scheme)" + version_str
    suit_symbols = rd_suits + ' --suit_colors=' + rd_dark
    file = "rainbow_deck1"
    extra_flags = "" + rd_background + dozenal_chips
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    deck_title = " --deck_title='RD-suited piecepack (Light color scheme)" + version_str
    suit_symbols = rd_suits + ' --suit_colors=' + rd_light
    file = "rainbow_deck2"
    extra_flags = "" + rd_background + dozenal_chips
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    decks = " --decks=rainbow_deck1,rainbow_deck2"
    sh collect_piecepacks + " --filename=rainbow_deck_demo --title='Rainbow Deck suited piecepack demo" + version_str + decks + " --subject='The Rainbow Deck (RD) is a cardgame system by Chen Changcai.  More information is available at https://boardgamegeek.com/boardgame/59655/rainbow-deck.'"
end

desc "Reversi-friendly piecepacks demo"
task :reversi do
    deck_title = " --deck_title='Piecepack-suited" + version_str
    suit_symbols = piecepack_suits 
    file = "reversi1"
    extra_flags = " --background_color.suited=tan3 --background_color.chip_back=white"
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    deck_title = " --deck_title='Elements-suited" + version_str
    suit_symbols = alchemical_elements
    file = "reversi2"
    extra_flags = " --background_color.suited=black --background_color.chip_back=white"
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    deck_title = " --deck_title='Sixpack (\"Black\" suits)" + version_str
    suit_symbols = sixpack_suits_black + " --suit_colors=black,black,black,black,black,black,grey"
    file = "reversi3"
    extra_flags = " --suit_colors.chip_back=grey" + dozenal_chips
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    deck_title = " --deck_title='Sixpack (\"White\" suits)" + version_str
    suit_symbols = sixpack_suits_white + " --suit_colors=black,black,black,black,black,black,grey"
    file = "reversi4"
    extra_flags = " --suit_colors.chip_back=grey" + dozenal_chips
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    deck_title = " --deck_title='Red-suited " + version_str
    suit_symbols = red_suits6 + " --suit_colors=white --background_color.suited=darkred --background_color.unsuited=black"
    file = "reversi5"
    extra_flags = " --background_color.chip_back=black" + dozenal_chips
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    deck_title = " --deck_title='Black-suited " + version_str
    suit_symbols = black_suits6 + " --suit_colors=white --background_color.suited=black --background_color.unsuited=darkred"
    file = "reversi6"
    extra_flags = " --background_color.chip_back=darkred" + dozenal_chips
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    decks = " --decks=reversi1,reversi2,reversi3,reversi4,reversi5,reversi6"
    sh collect_piecepacks + " --filename=reversi_demo --title='Reversi-friendly piecepacks demo" + version_str + decks
end

desc "Sixpack piecepacks demo"
task :sixpack do
    deck_title = " --deck_title='Sixpack" + version_str
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,black,darkred,darkred,black,grey'
    file = "sixpack1"
    extra_flags = ""
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    deck_title = " --deck_title='Sixpack ('Orthodox' style)" + version_str
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,black,darkred,darkred,black,black'
    file = "sixpack2"
    extra_flags = "" + pyramid_configuration + orthodox_dm + orthodox_saucers3
    make_piecepack file, deck_title + suit_symbols + orthodox_ranks + extra_flags 

    deck_title = " --deck_title='Sixpack (Dark multicolor scheme)" + version_str
    # sixpack_suits2 = " --suit_symbols=♥,♤,♣,♦,🌞,🌜,꩜ --suit_symbols_scale=1,1,1,1,0.7,0.8,1.2,1.2"
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,darkgreen,darkblue,orange3,purple,tan4'
    file = "sixpack3"
    extra_flags = " --background_color=seashell3" + dozenal_chips
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    deck_title = " --deck_title='Sixpack (Light multicolor scheme)" + version_str
    suit_symbols = sixpack_suits + ' --suit_colors=hotpink2,dimgrey,palegreen,lightblue1,yellow,white,tan4'
    file = "sixpack4"
    extra_flags = " --background_color=seashell3" + dozenal_chips
    make_piecepack file, deck_title + suit_symbols + default_ranks + extra_flags 

    decks = " --decks=sixpack1,sixpack2,sixpack3,sixpack4"
    sh collect_piecepacks + " --filename=sixpack_demo --title='Sixpack demo" + version_str + decks
end

## Development Utilities
desc "Install"
task :install do
    sh 'sudo Rscript -e "suppressMessages(devtools::document())"'
    sh 'sudo Rscript -e "devtools::install(quiet=TRUE, upgrade_dependencies=FALSE)"'
    # sh 'exec/configure_piecepack --help | sed "s/^/| /" > configurations/configure_piecepack_options.txt'
    sh 'exec/configure_piecepack --help | cat > txt/configure_piecepack_options.txt | true'
    sh 'exec/make_preview --help | cat > txt/make_preview_options.txt'
    sh 'exec/make_piecepack --help | cat > txt/make_piecepack_options.txt'
    sh 'exec/collect_piecepacks --help | cat > txt/collect_piecepacks_options.txt'
end

def open_pdf (demo)
    sh "xdg-open pdf/collections/" + demo + "_demo.pdf 2>/dev/null | true"
end

desc "Test"
task :test => :clean
task :test => :install
task :test do
    extra_flags = " --use_suit_as_ace --rank_symbols.chip_face='A,B,C,D,E,F'"
    file = "test1"
    make_piecepack file,  piecepack_suits + orthodox_ranks + extra_flags 

    extra_flags = "  --invert_colors.suited  --background_color=white" + star_chips
    file = "test2"
    make_piecepack file, latin_suits_swirl + default_ranks + extra_flags

    # suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    # extra_flags = " --dm_symbols= --dm_symbols.tile_face=♥,♠,♣,♦,★ --background_color.unsuited=orange" 
    # file = "test3"
    # make_piecepack file, suit_symbols + default_ranks + extra_flags

    # suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    # extra_flags = " --suit_colors=" + light_scheme + " --invert_colors.unsuited --background_color=white --font='Deja Vu Sans'" + hexlines_light
    # file = "test4"
    # make_piecepack file, suit_symbols + default_ranks + extra_flags

    # decks = " --decks=test1,test2,test3,test4"
    decks = " --decks=test1,test2"
    sh collect_piecepacks + " --filename=test --title='Test" + version_str + decks
    open_pdf "test"
end

desc "Open demos"
task :open_demos do
    demos.each { |demo| open_pdf "#{demo}" }
end

desc "Copy demos over to Dropbox"
task :copy_demos do
    dir = "/home/trevorld/a/sync/Dropbox/Public/piecepack/"
    demos.each { |x| sh "cp pdf/collections/#{x}_demo.pdf #{dir}" }
end
