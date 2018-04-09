desc "Clean"
task :clean do
    rm_r "collections"
    mkdir "collections"
    rm_r "previews"
    mkdir "previews"
    rm_r "svg"
    mkdir "svg"
    rm_r "pdf"
    mkdir "pdf"
    sh "killall evince | true"
end

version_str = " (v" + `Rscript -e 'cat(packageDescription("piecepack")$Version)'` + ")'"

configure_piecepack = "exec/configure_piecepack --font='Noto Sans Symbols' --header_font='Noto Sans' "
collect_piecepacks = "exec/collect_piecepacks --font='Noto Sans' --author='Trevor L Davis' "

# Colors
# --background_color=seashell3")
# dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue"
# alt_scheme = " --suit_colors=white,orange2,yellow,purple"
chinese_elements_colors = " --suit_colors=darkgreen,red3,chocolate4,black,darkblue,grey"
light = "hotpink2,dimgrey,palegreen,lightblue2,"
dark = "darkred,black,darkgreen,darkblue,"
light_scheme = light + "grey"
dark_scheme = dark + "grey"
hexlines_dark = " --hexline_colors=" + dark + "transparent"
hexlines_light = " --hexline_colors=" + light+ "transparent"
rd_dark = "#ff0000,#ffc000,#008000,#0000ff,#800080,#000000,white"
rd_light = "#ff80c0,#ffff80,#80ff00,#80ffff,#c080ff,#c0c0c0,white"

# Suits
french_suits = " --suit_symbols=♥,♠,♣,♦,★ --suit_symbols_scale=1,1,1,1,1,1 --suit_symbols_font='Noto Sans Symbols'"
french_suits_swirl = " --suit_symbols=♥,♠,♣,♦,꩜ --suit_symbols_scale=1,1,1,1,1.2,1.2"
# latin_suits_swirl = " --suit_symbols=🏆,🗡️,⚕️,𐇛,꩜ --suit_symbols_scale=0.6,1,1,1.1,1.2"
latin_suits_swirl = " --suit_symbols=,,,,꩜ --suit_symbols_font=Quivira --suit_symbols_scale=1,1,1,1,1.2"
piecepack_suits = " --suit_symbols=🌞,🌜,👑,⚜,꩜ --suit_symbols_scale=0.7,0.8,0.8,1.1,1.2,1.2 --suit_symbols_font='Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham'"
rd_suits = " --suit_symbols=♥,★,♣,♦,♛,♠,꩜ --suit_symbols_scale=1,1,1,1,0.85,1,1.2 --suit_symbols_font='Noto Sans Symbols'"
# swiss_suits = " --suit_symbols=🌹,⛊,🌰,🔔,★ --suit_symbols_scale=0.7,1.0,0.8,0.8,1.2,1.2"
swiss_suits = " --suit_symbols=,,,, --suit_symbols_font=Quivira --suit_symbols_scale=1"
sixpack_suits = " --suit_symbols=♥,♠,♣,♦,🌞,🌜,꩜ --suit_symbols_scale=1,1,1,1,0.8,0.8,1.2,1.2 --suit_symbols_font='Noto Emoji'"
chinese_elements = " --suit_symbols=🌲,🔥,⛰️,🏆,🌊, --suit_symbols_scale=1.1,1.1,1,0.6,0.6 --suit_symbols_font='Quivira,Quivira,Quivira,Noto Sans Symbols2,Noto Emoji,'" + chinese_elements_colors
chinese_elements2 = " --suit_symbols=木,火,土,金,水, --suit_symbols_scale=0.7 --suit_symbols_font='Noto Sans CJK SC'" + chinese_elements_colors 

# Ranks
default_ranks = " --rank_symbols=N,A,2,3,4,5 --rank_symbols_font='Noto Sans'"
orthodox_ranks = " --rank_symbols=,A,2,3,4,5 --use_suit_as_ace --rank_symbols_font='Noto Sans'"

# Configurations
pyramid_configuration = " --rank_symbols.chip_face='A,B,C,D,E,F' --use_ace_as_ace.chip_face --dm_symbols.chip_face= --dm_symbols.chip_back= --dm_symbols_font.chip_face='Noto Sans', --dm_symbols_font.chip_back='Noto Sans'"
orthodox_dm = " --dm_colors.coin_face=black --dm_colors.coin_back=black --dm_symbols.piecepack_die= --dm_symbols_font.piecepack_die='Noto Sans' --dm_symbols.coin_face='|' --dm_symbols_font.coin_face='Noto Sans' --dm_r.coin_face=0.45 --dm_symbols.coin_back='|' --dm_symbols_font.coin_back='Noto Sans' --dm_r.coin_back=0.45"
# ○ ⚆ ⚇ ● ⚈ ⚉
orthodox_saucers = " --suit_symbols.saucer_back= --dm_colors.saucer_face=black --dm_symbols.saucer_face='|' --dm_symbols_font.saucer_face='Noto Sans' --dm_r.saucer_face=0.45 --dm_symbols.saucer_back='|' --dm_symbols_font.saucer_back='Noto Sans' --dm_r.saucer_back=0.45"

orthodox_saucers1 = " --suit_symbols.saucer_face=● --suit_symbols_scale.saucer_face=1 --suit_symbols_font.saucer_face='Noto Sans Symbols'" + orthodox_saucers
orthodox_saucers2 = " --suit_symbols.saucer_face=⚈,⚉,⚈,⚉, --suit_symbols_scale.saucer_face=0.7 --suit_symbols_font.saucer_face='Noto Sans Symbols'" + orthodox_saucers
orthodox_saucers3 = " --suit_symbols.saucer_face=⚈,⚈,⚉,⚉,●,●, --suit_symbols_scale.saucer_face=0.7,0.7,0.7,0.7,1,1 --suit_symbols_font.saucer_face='Noto Sans Symbols'" + orthodox_saucers

## Demos
desc "Run all demos"
task :all do
    Rake::Task[:chinese_zodiac].invoke()
    Rake::Task[:default].invoke()
    Rake::Task[:dual].invoke()
    Rake::Task[:orthodox].invoke()
    Rake::Task[:rainbow_deck].invoke()
    Rake::Task[:sixpack].invoke()
end

desc "Chinese zodiac demo"
task :chinese_zodiac do
    deck_title = " --deck_title='Chinese zodiac #1" + version_str
    deck_filename = " --deck_filename=chinese_zodiac1"
    suit_symbols = chinese_elements
    rank_symbols = " --rank_symbols=🐀,🐉,🐒,🐂,🐍,🐓 --rank_symbols_scale=0.6 --rank_symbols_font='Noto Emoji'"
    file1 = "configurations/chinese_zodiac1.json"
    extra_flags = " --file=" + file1  
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file1

    deck_title = " --deck_title='Chinese zodiac #2" + version_str
    deck_filename = " --deck_filename=chinese_zodiac2"
    suit_symbols = chinese_elements
    rank_symbols = " --rank_symbols=🐅,🐎,🐕,🐇,🐏,🐖 --rank_symbols_scale=0.6 --rank_symbols_font='Noto Emoji'"
    file2 = "configurations/chinese_zodiac2.json"
    extra_flags = " --file=" + file2 
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags 
    sh "exec/make_piecepack --file=" + file2 

    deck_title = " --deck_title='Chinese zodiac #3" + version_str
    deck_filename = " --deck_filename=chinese_zodiac3"
    suit_symbols = chinese_elements2
    rank_symbols = " --rank_symbols=鼠,龙,猴,牛,蛇,鸡 --rank_symbols_font='Noto Sans CJK SC' --rank_symbols_scale=0.7"
    file1 = "configurations/chinese_zodiac3.json"
    extra_flags = " --file=" + file1  
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file1

    deck_title = " --deck_title='Chinese zodiac #4" + version_str
    deck_filename = " --deck_filename=chinese_zodiac4"
    suit_symbols = chinese_elements2
    rank_symbols = " --rank_symbols=虎,马,狗,兔,羊,猪 --rank_symbols_font='Noto Sans CJK SC' --rank_symbols_scale=0.7"
    file2 = "configurations/chinese_zodiac4.json"
    extra_flags = " --file=" + file2 
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags 
    sh "exec/make_piecepack --file=" + file2 

    decks = " --decks=chinese_zodiac1,chinese_zodiac2,chinese_zodiac3,chinese_zodiac4"
    sh collect_piecepacks + " --filename=chinese_zodiac_demo --title='Chinese zodiac demo" + version_str + decks
    sh "xdg-open collections/chinese_zodiac_demo.pdf"
end

desc "Default piecepack demo"
task :default do
    file1 = "configurations/default.json"
    extra_flags = " --font='Noto Sans Symbols' --header_font='Noto Sans' --rank_symbols_font='Noto Sans' --file=" + file1
    sh "exec/configure_piecepack" + extra_flags
    sh "exec/make_piecepack --file=" + file1
    decks = " --decks=piecepack_deck"
    sh "exec/collect_piecepacks --font='Noto Sans'" + " --filename=default_demo --title='Default demo" + version_str + decks
    sh "xdg-open collections/default_demo.pdf"
end

desc "Dual piecepacks demo"
task :dual do
    # --background_color=seashell3")
    # dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue"
    # alt_scheme = " --suit_colors=white,orange2,yellow,purple"

    deck_filename = " --deck_filename=dual1piecepack"
    deck_title = " --deck_title='Piecepack-suited (Un-inverted color scheme)" + version_str
    file1 = "configurations/dual1_uninverted_piecepack.json"
    extra_flags = " --file=" + file1 
    sh configure_piecepack + deck_title + deck_filename + piecepack_suits + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file1

    deck_filename = " --deck_filename=dual2latin"
    deck_title = " --deck_title='Latin-suited (Inverted color scheme)" + version_str
    file2 = "configurations/dual2_inverted_latin.json"
    extra_flags = "  --background_color=white --invert_colors.suited --file=" + file2 
    sh configure_piecepack + deck_title + deck_filename + latin_suits_swirl + default_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file2

    deck_filename = " --deck_filename=dual3french"
    deck_title = " --deck_title='French-suited (Dark color scheme)" + version_str
    file3 = "configurations/dual3_dark_french.json"
    extra_flags = " --file=" + file3 + hexlines_dark
    sh configure_piecepack + deck_title + deck_filename + french_suits_swirl + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file3

    deck_filename = " --deck_filename=dual4french"
    deck_title = " --deck_title='French-suited (Light color scheme)" + version_str
    file4 = "configurations/dual4_light_french.json"
    extra_flags = " --file=" + file4 + " --suit_colors=" + light_scheme + hexlines_light
    sh configure_piecepack + deck_title + deck_filename + french_suits_swirl + default_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file4

    deck_filename = " --deck_filename=dual5swiss"
    deck_title = " --deck_title='Swiss-suited (Black color scheme)" + version_str
    black_scheme = " --suit_colors=black,black,black,black,grey40 --background_color=grey70"
    file5 = "configurations/dual5_black_swiss.json"
    extra_flags = " --invert_colors.suited --use_suit_as_ace --file=" + file5 + black_scheme
    sh configure_piecepack + deck_title + deck_filename + swiss_suits + orthodox_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file5

    deck_filename = " --deck_filename=dual6swiss"
    deck_title = " --deck_title='Swiss-suited (White color scheme)" + version_str
    white_scheme = " --suit_colors=white,white,white,white,grey40 --background_color=grey70"
    file6 = "configurations/dual6_white_swiss.json"
    extra_flags = " --invert_colors.suited --use_suit_as_ace  --file=" + file6 + white_scheme
    sh configure_piecepack + deck_title + deck_filename + swiss_suits + orthodox_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file6

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
    file1 = "configurations/orthodox1_piecepack.json"
    extra_flags = " --use_suit_as_ace --file=" + file1 + pyramid_configuration + orthodox_dm + orthodox_saucers1
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + orthodox_ranks + extra_flags
    sh "exec/make_piecepack --file=" + file1

    deck_title = " --deck_title='Orthodox-style 2-color french-suited piecepack" + version_str
    deck_filename = " --deck_filename=orthodox2_french"
    suit_symbols = french_suits_swirl + " --suit_colors=darkred,black,black,darkred,black "
    file2 = "configurations/orthodox2_french.json"
    extra_flags = " --use_suit_as_ace --file=" + file2 + pyramid_configuration + orthodox_dm + orthodox_saucers2
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + orthodox_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file2 

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
    file1 = "configurations/rainbow_deck1.json"
    extra_flags = " --file=" + file1 + rd_background
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file1 

    deck_title = " --deck_title='RD-suited piecepack (Light color scheme)" + version_str
    deck_filename = " --deck_filename=rainbow_deck2"
    suit_symbols = rd_suits + ' --suit_colors=' + rd_light
    file2 = "configurations/rainbow_deck2.json"
    extra_flags = " --file=" + file2 + rd_background
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file2 

    decks = " --decks=rainbow_deck1,rainbow_deck2"
    sh collect_piecepacks + " --filename=rainbow_deck_demo --title='Rainbow Deck suited piecepack demo" + version_str + decks + " --subject='The Rainbow Deck (RD) is a cardgame system by Chen Changcai.  More information is available at https://boardgamegeek.com/boardgame/59655/rainbow-deck.'"
    sh "xdg-open collections/rainbow_deck_demo.pdf"
end

desc "Sixpack piecepacks demo"
task :sixpack do
    deck_title = " --deck_title='Sixpack-suited piecepack" + version_str
    deck_filename = " --deck_filename=sixpack1"
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,black,darkred,darkred,black,grey'
    file1 = "configurations/sixpack1.json"
    extra_flags = " --file=" + file1
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file1 

    deck_title = " --deck_title='Sixpack-suited piecepack ('Orthodox' style)" + version_str
    deck_filename = " --deck_filename=sixpack2"
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,black,darkred,darkred,black,black'
    file2 = "configurations/sixpack2.json"
    extra_flags = " --file=" + file2 + pyramid_configuration + orthodox_dm + orthodox_saucers3
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + orthodox_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file2 

    deck_title = " --deck_title='Sixpack-suited piecepack (Dark multicolor scheme)" + version_str
    deck_filename = " --deck_filename=sixpack3"
    # sixpack_suits2 = " --suit_symbols=♥,♤,♣,♦,🌞,🌜,꩜ --suit_symbols_scale=1,1,1,1,0.7,0.8,1.2,1.2"
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,darkgreen,darkblue,orange3,purple,tan4'
    file3 = "configurations/sixpack3.json"
    extra_flags = " --background_color=seashell3 --file=" + file3
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file3 

    deck_title = " --deck_title='Sixpack-suited piecepack (Light multicolor scheme)" + version_str
    deck_filename = " --deck_filename=sixpack4"
    suit_symbols = sixpack_suits + ' --suit_colors=hotpink2,dimgrey,palegreen,lightblue1,yellow,white,tan4'
    file4 = "configurations/sixpack4.json"
    extra_flags = " --background_color=seashell3 --file=" + file4
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + default_ranks + extra_flags 
    sh "exec/make_piecepack --file=" + file4 

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
    # dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue,grey"
    # alt_scheme = " --suit_colors=white,orange2,yellow,purple,grey"

    deck_filename = " --deck_filename=test1piecepack"
    extra_flags = " --use_suit_as_ace  --rank_symbols.chip_face='A,B,C,D,E,F'"
    sh configure_piecepack + deck_filename + piecepack_suits + orthodox_ranks + extra_flags + ' | exec/make_piecepack'

    # deck_name = " --deck_name='TLD Piecepack, Elements Suits (v0.1)'"
    # deck_filename  = " --deck_filename=elements"
    # suit_symbols = " --suit_symbols=🔥,🌪️,⛰️,🌊,"
      
    deck_filename = " --deck_filename=test2latin"
    extra_flags = "  --invert_colors.suited  "
    sh configure_piecepack + deck_filename + latin_suits_swirl + default_ranks + extra_flags + ' | exec/make_piecepack'

    deck_filename = " --deck_filename=test3french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    extra_flags = " --dm_symbols= --dm_symbols.tile_face=♥,♠,♣,♦,★ --background_color.unsuited=orange" 
    sh configure_piecepack + deck_filename + suit_symbols + default_ranks + extra_flags + ' | exec/make_piecepack'

    deck_filename = " --deck_filename=test4french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    extra_flags = " --suit_colors=" + light_scheme + " --invert_colors.unsuited --font='Deja Vu Sans'" + hexlines_light
    sh 'exec/configure_piecepack' + deck_filename + suit_symbols + default_ranks + extra_flags + ' | exec/make_piecepack'

    decks = " --decks=test1piecepack,test2latin,test3french,test4french"
    sh collect_piecepacks + " --filename=test --title='Test'" + version_str + decks
    sh "xdg-open collections/test.pdf"

    # deck_name = " --deck_name='TLD Euchre Piecepack, French Suits (v0.1)'"
    # deck_filename = " --deck_filename=french"
    # suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    # rank_symbols = " --rank_symbols=9,10,J,Q,K,A"
      
    # deck_name = " --deck_name='TLD Chess Piecepack (v0.1)'"
    # deck_filename = " --deck_filename=chess"
    # # suit_symbols = " --suit_symbols=♟,♟,♙,♙,"
    # suit_symbols = " --suit_symbols=♙,♙,♙,♙,♙"
    # rank_symbols = " --rank_symbols=♞,♟,♝,♜,♛,♚"
    # rank_symbols = " --rank_symbols=♘,♙,♗,♖,♕,♔"
    # # add_checkers = TRUE
      
    # deck_name = " --deck_name='TLD Alchemical Piecepack, Elemental Suits (v0.1)'"
    # deck_filename = " --deck_filename=alchemical"
    # suit_symbols = " --suit_symbols=🜁,🜂,🜃,🜄,"
    # # rank_symbols = "  --rank_symbols=' ,𝍩,𝍪,𝍫,𝍬,𝍭'"
    # # rank_symbols = " --rank_symbols=' ,🝔,:,∴,⸪,⁙"
    # # rank_symbols = " --rank_symbols=' 🝔,🜩,☰,⚏,☵"
    # rank_symbols = " --rank_symbols=' 🝪,🜩,ʒ,♃,🜪"
end

desc "Copy demos over to Dropbox"
task :copy_demos do
    sh "cp collections/chinese_zodiac_demo.pdf /home/trevorld/a/sync/Dropbox/Public/piecepack/"
    sh "cp collections/default_demo.pdf /home/trevorld/a/sync/Dropbox/Public/piecepack/"
    sh "cp collections/dual_demo.pdf /home/trevorld/a/sync/Dropbox/Public/piecepack/"
    sh "cp collections/orthodox_demo.pdf /home/trevorld/a/sync/Dropbox/Public/piecepack/"
    sh "cp collections/rainbow_deck_demo.pdf /home/trevorld/a/sync/Dropbox/Public/piecepack/"
    sh "cp collections/sixpack_demo.pdf /home/trevorld/a/sync/Dropbox/Public/piecepack/"
end
