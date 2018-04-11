desc "Clean"
task :clean do
    dirs = ["collections", "previews", "svg", "pdf"]
    dirs.each { |dir|
        rm_r "#{dir}"
        mkdir "#{dir}"
        sh "touch #{dir}/.gitginore"
    }
    sh "killall evince | true"
end

demos = ["chess", "chinese_zodiac", "crown_and_anchor", "default",
         "dual", "orthodox", "rainbow_deck", "reversi"]

version_str = " (v" + `Rscript -e 'cat(packageDescription("piecepack")$Version)'` + ")'"

configure_piecepack = "exec/configure_piecepack --font='Noto Sans Symbols' --header_font='Noto Sans' "
collect_piecepacks = "exec/collect_piecepacks --font='Noto Sans' --author='Trevor L Davis' "

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
coin_red_suits4 = " --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1_--suit_symbols_font.coin_back='Noto Sans Symbols' --dm_symbols.coin_back=♥,♦,☉,♚,★  --dm_symbols_font.coin_back='Noto Sans Symbols'"
red_suits6 = " --suit_symbols=♥,♦,🌞,♚,,,★  --suit_symbols_font='Noto Sans Symbols,Noto Sans Symbols,Noto Emoji,Noto Sans Symbols,Quivira,Quivira,Noto Sans Symbols' --suit_symbols_scale=1,1,0.8,1,1,1,1"
coin_red_suits6 = " --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1 --suit_symbols_font.coin_back='Noto Sans Symbols' --dm_symbols.coin_back=♥,♦,🌞,♚,,,★  --dm_symbols_font.coin_back='Noto Sans Symbols,Noto Sans Symbols,Noto Emoji,Noto Sans Symbols,Quivira,Quivira,Noto Sans Symbols' --dm_symbols_scale.coin_back=1,1,0.8,1,1,1,1"
alchemical_elements = " --suit_symbols=🜁,🜂,🜃,🜄, --suit_colors=red,khaki2,green,cornflowerblue,grey"
# emoji_elements = " --suit_symbols=🔥,🌪️,⛰️,🌊, --suit_colors=darkred,black,darkgreen,darkblue"
chinese_elements = " --suit_symbols=🌲,🔥,⛰️,🏆,🌊, --suit_symbols_scale=1.1,1.1,1,0.6,0.6 --suit_symbols_font='Quivira,Quivira,Quivira,Noto Sans Symbols2,Noto Emoji,'" + chinese_elements_colors
chinese_elements2 = " --suit_symbols=木,火,土,金,水, --suit_symbols_scale=0.7 --suit_symbols_font='Noto Sans CJK SC'" + chinese_elements_colors 

# Ranks
default_ranks = " --rank_symbols=N,A,2,3,4,5 --rank_symbols_font='Noto Sans'"
orthodox_ranks = " --rank_symbols=,A,2,3,4,5 --use_suit_as_ace --rank_symbols_font='Noto Sans'"
# chess_ranks = " --rank_symbols=P,N,B,R,Q,K --rank_symbols_font='Chess Utrecht' --rank_symbols_scale=1.3"
chess_ranks = " --rank_symbols=♞,♟,♝,♜,♛,♚ --rank_symbols_font='Sans Noto Symbols'"
# chess_ranks = " --rank_symbols=♘,♙,♗,♖,♕,♔"

# Configurations
pyramid_configuration = " --rank_symbols.chip_face='A,B,C,D,E,F' --use_ace_as_ace.chip_face --dm_symbols.chip_face= --dm_symbols.chip_back= "
orthodox_dm = " --dm_colors.coin_face=black --dm_colors.coin_back=black --dm_symbols.piecepack_die= --dm_symbols.coin_face='|' --dm_symbols_font.coin_face='Noto Sans' --dm_r.coin_face=0.45 --dm_symbols.coin_back='|' --dm_symbols_font.coin_back='Noto Sans' --dm_r.coin_back=0.45"
# ○ ⚆ ⚇ ● ⚈ ⚉
orthodox_saucers = " --suit_symbols.saucer_back= --dm_colors.saucer_face=black --dm_symbols.saucer_face='|' --dm_symbols_font.saucer_face='Noto Sans' --dm_r.saucer_face=0.45 --dm_symbols.saucer_back='|' --dm_symbols_font.saucer_back='Noto Sans' --dm_r.saucer_back=0.45"

orthodox_saucers1 = " --suit_symbols.saucer_face=● --suit_symbols_scale.saucer_face=1 --suit_symbols_font.saucer_face='Noto Sans Symbols'" + orthodox_saucers
orthodox_saucers2 = " --suit_symbols.saucer_face=⚈,⚉,⚈,⚉, --suit_symbols_scale.saucer_face=0.7 --suit_symbols_font.saucer_face='Noto Sans Symbols'" + orthodox_saucers
orthodox_saucers3 = " --suit_symbols.saucer_face=⚈,⚈,⚉,⚉,●,●, --suit_symbols_scale.saucer_face=0.7,0.7,0.7,0.7,1,1 --suit_symbols_font.saucer_face='Noto Sans Symbols'" + orthodox_saucers

## Demos
desc "Run all demos"
task :all do
    demos.each { |demo|
        Rake::Task["#{demo}"].invoke()
    }
end

desc "Chinese zodiac demo"
task :chinese_zodiac do
    deck_title = " --deck_title='Chinese zodiac #1" + version_str
    deck_filename = " --deck_filename=chinese_zodiac1"
    suit_symbols = chinese_elements
    rank_symbols = " --rank_symbols=🐀,🐉,🐒,🐂,🐍,🐓 --rank_symbols_scale=0.6 --rank_symbols_font='Noto Emoji'"
    file = "configurations/chinese_zodiac1.json"
    extra_flags = " --file=" + file  
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_title = " --deck_title='Chinese zodiac #2" + version_str
    deck_filename = " --deck_filename=chinese_zodiac2"
    suit_symbols = chinese_elements
    rank_symbols = " --rank_symbols=🐅,🐎,🐕,🐇,🐏,🐖 --rank_symbols_scale=0.6 --rank_symbols_font='Noto Emoji'"
    file = "configurations/chinese_zodiac2.json"
    extra_flags = " --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='Chinese zodiac #3" + version_str
    deck_filename = " --deck_filename=chinese_zodiac3"
    suit_symbols = chinese_elements2
    rank_symbols = " --rank_symbols=鼠,龙,猴,牛,蛇,鸡 --rank_symbols_font='Noto Sans CJK SC' --rank_symbols_scale=0.7"
    file = "configurations/chinese_zodiac3.json"
    extra_flags = " --file=" + file  
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_title = " --deck_title='Chinese zodiac #4" + version_str
    deck_filename = " --deck_filename=chinese_zodiac4"
    suit_symbols = chinese_elements2
    rank_symbols = " --rank_symbols=虎,马,狗,兔,羊,猪 --rank_symbols_font='Noto Sans CJK SC' --rank_symbols_scale=0.7"
    file = "configurations/chinese_zodiac4.json"
    extra_flags = " --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    decks = " --decks=chinese_zodiac1,chinese_zodiac2,chinese_zodiac3,chinese_zodiac4"
    sh collect_piecepacks + " --filename=chinese_zodiac_demo --title='Chinese zodiac demo" + version_str + decks
    sh "xdg-open collections/chinese_zodiac_demo.pdf"
end

desc "Chess demo"
task :chess do
    deck_filename = " --deck_filename=chess1"
    deck_title = " --deck_title='Piecepack-suits" + version_str
    file = "configurations/chess1.json"
    extra_flags = " --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + piecepack_suits + chess_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=chess2"
    deck_title = " --deck_title='French-suits (checkered tile faces)" + version_str
    file = "configurations/chess2.json"
    extra_flags = " --checker_colors=grey,grey,grey,grey, --suit_colors=darkred,black,dimgrey,hotpink2,grey --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + french_suits_swirl + chess_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=chess3"
    deck_title = " --deck_title='Black-suits (checkered tile backs)" + version_str
    file = "configurations/chess3.json"
    extra_flags = " --checker_colors=,,,,darkred --gridline_colors=  --suit_colors=black,black,black,black,darkred --file=" + file + coin_black_suits4
    sh configure_piecepack + deck_title + deck_filename + black_suits4 + chess_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=chess4"
    deck_title = " --deck_title='Red-suits (checkered tile backs)" + version_str
    file = "configurations/chess4.json"
    extra_flags = " --checker_colors=,,,,black --gridline_colors= --suit_colors=darkred,darkred,darkred,darkred,black --file=" + file + coin_red_suits4
    sh configure_piecepack + deck_title + deck_filename + red_suits4 + chess_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=chess5"
    deck_title = " --deck_title='Black-suits (checkered tile faces)" + version_str
    file = "configurations/chess5.json"
    extra_flags = " --checker_colors=grey,grey,grey,grey,grey,grey, --suit_colors=black,black,black,black,black,black,darkred --file=" + file + coin_black_suits6
    sh configure_piecepack + deck_title + deck_filename + black_suits6 + chess_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=chess6"
    deck_title = " --deck_title='Red-suits (checkered tile faces)" + version_str
    file = "configurations/chess6.json"
    extra_flags = " --checker_colors=grey,grey,grey,grey,grey,grey, --suit_colors=darkred,darkred,darkred,darkred,darkred,darkred,black --file=" + file + coin_red_suits6
    sh configure_piecepack + deck_title + deck_filename + red_suits6 + chess_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file

    decks = " --decks=chess1,chess2,chess3,chess4,chess5,chess6"
    sh collect_piecepacks + " --filename=chess_demo --title='Chess ranked piecepack demo" + version_str + decks
    sh "xdg-open collections/chess_demo.pdf"

end

desc "Crown and anchor demo"
task :crown_and_anchor do

    deck_filename = " --deck_filename=crown_and_anchor1"
    deck_title = " --deck_title='Crown and anchor suits" + version_str
    file = "configurations/crown_and_anchor1.json"
    extra_flags = " --suit_colors=darkred,darkred,black,black,darkred,black,grey --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + ca_suits + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=crown_and_anchor2"
    deck_title = " --deck_title='Crown and anchor suits (Multicolor)" + version_str
    file = "configurations/crown_and_anchor2.json"
    extra_flags = " --suit_colors=darkred,darkblue,darkgreen,black,purple,orange2,grey --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + ca_suits + default_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=crown_and_anchor3"
    deck_title = " --deck_title='JCD piecepack suits (Monoscale)" + version_str
    file = "configurations/crown_and_anchor3.json"
    extra_flags = " --suit_colors=black --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + jcd_suits + default_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=crown_and_anchor4"
    deck_title = " --deck_title='JCD piecepack suits (Black/Red)" + version_str
    file = "configurations/crown_and_anchor4.json"
    extra_flags = " --suit_colors=darkred,black,darkred,black,grey --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + jcd_suits + default_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=crown_and_anchor5"
    deck_title = " --deck_title='JCD piecepack suits (Multicolor)" + version_str
    file = "configurations/crown_and_anchor5.json"
    extra_flags = " --suit_colors=darkred,black,darkgreen,darkblue,grey --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + jcd_suits + default_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=crown_and_anchor6"
    deck_title = " --deck_title='JCD piecepack suits (Alternative Multicolor)" + version_str
    file = "configurations/crown_and_anchor6.json"
    extra_flags = " --suit_colors=yellow,white,purple,orange2,tan4 --background_color=seashell3 --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + jcd_suits + default_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    title = " --title='\"Crown and anchor\" demo" + version_str
    decks = " --decks=crown_and_anchor1,crown_and_anchor2,crown_and_anchor3,crown_and_anchor4,crown_and_anchor5,crown_and_anchor6"
    sh collect_piecepacks + " --filename=crown_and_anchor_demo" + title + decks
    sh "xdg-open collections/crown_and_anchor_demo.pdf"
end

desc "Default piecepack demo"
task :default do
    file = "configurations/default.json"
    extra_flags = " --font='Noto Sans Symbols' --header_font='Noto Sans' --rank_symbols_font='Noto Sans' --file=" + file
    sh "exec/configure_piecepack" + extra_flags
    sh "exec/make_piecepack --file=" + file
    decks = " --decks=piecepack_deck"
    sh "exec/collect_piecepacks --font='Noto Sans'" + " --filename=default_demo --title='Default demo" + version_str + decks
    sh "xdg-open collections/default_demo.pdf"
end


desc "Dual piecepacks demo"
task :dual do

    deck_filename = " --deck_filename=dual1piecepack"
    deck_title = " --deck_title='Piecepack-suited (Un-inverted color scheme)" + version_str
    file = "configurations/dual1_uninverted_piecepack.json"
    extra_flags = " --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + piecepack_suits + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=dual2latin"
    deck_title = " --deck_title='Latin-suited (Inverted color scheme)" + version_str
    file = "configurations/dual2_inverted_latin.json"
    extra_flags = "  --background_color=white --invert_colors.suited --file=" + file 
    sh configure_piecepack + deck_title + deck_filename + latin_suits_swirl + default_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=dual3french"
    deck_title = " --deck_title='French-suited (Dark color scheme)" + version_str
    file = "configurations/dual3_dark_french.json"
    extra_flags = " --file=" + file + hexlines_dark
    sh configure_piecepack + deck_title + deck_filename + french_suits_swirl + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=dual4french"
    deck_title = " --deck_title='French-suited (Light color scheme)" + version_str
    file = "configurations/dual4_light_french.json"
    extra_flags = " --file=" + file + " --suit_colors=" + light_scheme + hexlines_light
    sh configure_piecepack + deck_title + deck_filename + french_suits_swirl + default_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=dual5swiss"
    deck_title = " --deck_title='Swiss-suited (Black color scheme)" + version_str
    black_scheme = " --suit_colors=black,black,black,black,grey40 --background_color=grey70"
    file = "configurations/dual5_black_swiss.json"
    extra_flags = " --invert_colors.suited --use_suit_as_ace --file=" + file + black_scheme
    sh configure_piecepack + deck_title + deck_filename + swiss_suits + orthodox_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_filename = " --deck_filename=dual6swiss"
    deck_title = " --deck_title='Swiss-suited (White color scheme)" + version_str
    white_scheme = " --suit_colors=white,white,white,white,grey40 --background_color=grey70"
    file = "configurations/dual6_white_swiss.json"
    extra_flags = " --invert_colors.suited --use_suit_as_ace  --file=" + file + white_scheme
    sh configure_piecepack + deck_title + deck_filename + swiss_suits + orthodox_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    title = " --title='\"Dual piecepacks\" proof of concept" + version_str
    decks = " --decks=dual1piecepack,dual2latin,dual3french,dual4french,dual5swiss,dual6swiss"
    sh collect_piecepacks + " --filename=dual_demo" + title + decks
    sh "xdg-open collections/dual_demo.pdf"
end

desc "Orthodox piecepacks demo"
task :orthodox do
    deck_title = " --deck_title='Orthodox piecepack" + version_str
    deck_filename = " --deck_filename=orthodox1_piecepack"
    suit_symbols = piecepack_suits + " --suit_colors=darkred,black,darkgreen,darkblue,black"
    file = "configurations/orthodox1_piecepack.json"
    extra_flags = " --use_suit_as_ace --file=" + file + pyramid_configuration + orthodox_dm + orthodox_saucers1
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + orthodox_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file

    deck_title = " --deck_title='Orthodox-style 2-color french-suited piecepack" + version_str
    deck_filename = " --deck_filename=orthodox2_french"
    suit_symbols = french_suits_swirl + " --suit_colors=darkred,black,black,darkred,black "
    file = "configurations/orthodox2_french.json"
    extra_flags = " --use_suit_as_ace --file=" + file + pyramid_configuration + orthodox_dm + orthodox_saucers2
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + orthodox_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    decks = " --decks=orthodox1_piecepack,orthodox2_french"
    sh collect_piecepacks + " --filename=orthodox_demo --title='Orthodox demo" + version_str + decks
    sh "xdg-open collections/orthodox_demo.pdf"
end

desc "Rainbow deck demo"
task :rainbow_deck do
    rd_background = " --background_color=sienna"
    deck_title = " --deck_title='RD-suited piecepack (Dark color scheme)" + version_str
    deck_filename = " --deck_filename=rainbow_deck1"
    suit_symbols = rd_suits + ' --suit_colors=' + rd_dark
    file = "configurations/rainbow_deck1.json"
    extra_flags = " --file=" + file + rd_background
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='RD-suited piecepack (Light color scheme)" + version_str
    deck_filename = " --deck_filename=rainbow_deck2"
    suit_symbols = rd_suits + ' --suit_colors=' + rd_light
    file = "configurations/rainbow_deck2.json"
    extra_flags = " --file=" + file + rd_background
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    decks = " --decks=rainbow_deck1,rainbow_deck2"
    sh collect_piecepacks + " --filename=rainbow_deck_demo --title='Rainbow Deck suited piecepack demo" + version_str + decks + " --subject='The Rainbow Deck (RD) is a cardgame system by Chen Changcai.  More information is available at https://boardgamegeek.com/boardgame/59655/rainbow-deck.'"
    sh "xdg-open collections/rainbow_deck_demo.pdf"
end

desc "Reversi-friendly piecepacks demo"
task :reversi do
    deck_title = " --deck_title='Piecepack-suited" + version_str
    deck_filename = " --deck_filename=reversi1"
    suit_symbols = piecepack_suits 
    file = "configurations/reversi1.json"
    extra_flags = " --background_color.suited=tan3 --background_color.chip_back=white --file=" + file
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='Elements-suited" + version_str
    deck_filename = " --deck_filename=reversi2"
    suit_symbols = alchemical_elements
    file = "configurations/reversi2.json"
    extra_flags = " --background_color.suited=black --background_color.chip_back=white --file=" + file
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='Sixpack-suited (\"Black\" suits)" + version_str
    deck_filename = " --deck_filename=reversi3"
    suit_symbols = sixpack_suits_black + " --suit_colors=black,black,black,black,black,black,grey"
    file = "configurations/reversi3.json"
    extra_flags = " --suit_colors.chip_back=grey --file=" + file
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='Sixpack-suited (\"White\" suits)" + version_str
    deck_filename = " --deck_filename=reversi4"
    suit_symbols = sixpack_suits_white + " --suit_colors=black,black,black,black,black,black,grey"
    file = "configurations/reversi4.json"
    extra_flags = " --suit_colors.chip_back=grey --file=" + file
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='Red-suited " + version_str
    deck_filename = " --deck_filename=reversi5"
    suit_symbols = red_suits6 + " --suit_colors=white --background_color.suited=darkred --background_color.unsuited=black"
    file = "configurations/reversi5.json"
    extra_flags = " --background_color.chip_back=black --file=" + file
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='Black-suited " + version_str
    deck_filename = " --deck_filename=reversi6"
    suit_symbols = black_suits6 + " --suit_colors=white --background_color.suited=black --background_color.unsuited=darkred"
    file = "configurations/reversi6.json"
    extra_flags = " --background_color.chip_back=darkred --file=" + file
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    decks = " --decks=reversi1,reversi2,reversi3,reversi4,reversi5,reversi6"
    sh collect_piecepacks + " --filename=reversi_demo --title='Reversi-friendly piecepacks demo" + version_str + decks
    sh "xdg-open collections/reversi_demo.pdf"
end

desc "Sixpack piecepacks demo"
task :sixpack do
    deck_title = " --deck_title='Sixpack-suited piecepack" + version_str
    deck_filename = " --deck_filename=sixpack1"
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,black,darkred,darkred,black,grey'
    file = "configurations/sixpack1.json"
    extra_flags = " --file=" + file
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='Sixpack-suited piecepack ('Orthodox' style)" + version_str
    deck_filename = " --deck_filename=sixpack2"
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,black,darkred,darkred,black,black'
    file = "configurations/sixpack2.json"
    extra_flags = " --file=" + file + pyramid_configuration + orthodox_dm + orthodox_saucers3
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + orthodox_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='Sixpack-suited piecepack (Dark multicolor scheme)" + version_str
    deck_filename = " --deck_filename=sixpack3"
    # sixpack_suits2 = " --suit_symbols=♥,♤,♣,♦,🌞,🌜,꩜ --suit_symbols_scale=1,1,1,1,0.7,0.8,1.2,1.2"
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,darkgreen,darkblue,orange3,purple,tan4'
    file = "configurations/sixpack3.json"
    extra_flags = " --background_color=seashell3 --file=" + file
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    deck_title = " --deck_title='Sixpack-suited piecepack (Light multicolor scheme)" + version_str
    deck_filename = " --deck_filename=sixpack4"
    suit_symbols = sixpack_suits + ' --suit_colors=hotpink2,dimgrey,palegreen,lightblue1,yellow,white,tan4'
    file = "configurations/sixpack4.json"
    extra_flags = " --background_color=seashell3 --file=" + file
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file 

    decks = " --decks=sixpack1,sixpack2,sixpack3,sixpack4"
    sh collect_piecepacks + " --filename=sixpack_demo --title='Sixpack demo" + version_str + decks
    sh "xdg-open collections/sixpack_demo.pdf"
end

## Development Utilities
desc "Install"
task :install do
    sh 'sudo Rscript -e "suppressMessages(devtools::document())"'
    sh 'sudo Rscript -e "devtools::install(quiet=TRUE, upgrade_dependencies=FALSE)"'
    # sh 'exec/configure_piecepack --help | sed "s/^/| /" > configurations/configure_piecepack_options.txt'
    sh 'exec/configure_piecepack --help | cat > man/configure_piecepack_options.txt | true'
    sh 'exec/make_piecepack --help | cat > man/make_piecepack_options.txt'
    sh 'exec/collect_piecepacks --help | cat > man/collect_piecepacks_options.txt'
end

desc "Test"
task :test => :clean
task :test => :install
task :test do
    deck_filename = " --deck_filename=test1"
    extra_flags = " --use_suit_as_ace  --rank_symbols.chip_face='A,B,C,D,E,F'"
    sh configure_piecepack + deck_filename + piecepack_suits + orthodox_ranks + extra_flags + ' | exec/make_piecepack'

      
    deck_filename = " --deck_filename=test2"
    extra_flags = "  --invert_colors.suited  --background_color=white"
    sh configure_piecepack + deck_filename + latin_suits_swirl + default_ranks + extra_flags + ' | exec/make_piecepack'

    deck_filename = " --deck_filename=test3"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    extra_flags = " --dm_symbols= --dm_symbols.tile_face=♥,♠,♣,♦,★ --background_color.unsuited=orange" 
    sh configure_piecepack + deck_filename + suit_symbols + default_ranks + extra_flags + ' | exec/make_piecepack'

    deck_filename = " --deck_filename=test4"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    extra_flags = " --suit_colors=" + light_scheme + " --invert_colors.unsuited --background_color=white --font='Deja Vu Sans'" + hexlines_light
    sh 'exec/configure_piecepack' + deck_filename + suit_symbols + default_ranks + extra_flags + ' | exec/make_piecepack'

    decks = " --decks=test1,test2,test3,test4"
    sh collect_piecepacks + " --filename=test --title='Test" + version_str + decks
    sh "xdg-open collections/test.pdf"
end

desc "Copy demos over to Dropbox"
task :copy_demos do
    dir = "/home/trevorld/a/sync/Dropbox/Public/piecepack/"
    demos.each { |x| sh "cp collections/#{x}_demo.pdf #{dir}" }
end
