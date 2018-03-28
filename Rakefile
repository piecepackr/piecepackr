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

configure_piecepack = "exec/configure_piecepack --font=Symbola "
pyramid_configuration = " --rank_symbols.chip_face='A,B,C,D,E,F' --use_ace_as_ace.chip_face --directional_mark_symbols.chip_face=,,,, --directional_mark_symbols.chip_back=,,,,"
light_scheme = " --suit_colors=hotpink2,dimgrey,palegreen,lightblue2,grey"
latin_suits = " --suit_symbols=🏆,🗡️,⚕️,𐇛,꩜ --suit_symbols_scale=0.6,1,1,1.1,1.2"
french_suits = " --suit_symbols=♥,♠,♣,♦,★ --suit_symbols_scale=1,1,1,1,1.2,1.2"
swiss_suits = " --suit_symbols=🌹,⛊,🌰,🔔,★ --suit_symbols_scale=0.7,1.0,0.8,0.8,1.2,1.2"
piecepack_suits = " --suit_symbols=🌞,🌜,👑,⚜,꩜ --suit_symbols_scale=0.7,0.8,0.8,1.1,1.2,1.2"
hexlines_dark = " --hexline_colors=darkred,black,darkgreen,darkblue,grey"
hexlines_light = " --hexline_colors=hotpink2,dimgrey,palegreen,lightblue2,grey"
orthodox_dm = " --directional_mark_colors.coin_face=black,black,black,black,black --directional_mark_colors.coin_back=black,black,black,black,black --directional_mark_symbols.piecepack_die=,,,,"
orthodox_saucers = " --suit_symbols.saucer_face=●,●,●,●, --suit_symbols_scale.saucer_face=0.5,0.5,0.5,0.5,0.5 --suit_symbols.saucer_back=●,●,●,●, --directional_mark_colors.saucer_face=black,black,black,black,black"
# --background_color=seashell3")
# dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue"
# alt_scheme = " --suit_colors=white,orange2,yellow,purple"

desc "Install"
task :install do
    sh 'sudo Rscript -e "suppressMessages(devtools::document())"'
    sh 'sudo Rscript -e "devtools::install(quiet=TRUE, upgrade_dependencies=FALSE)"'
    # sh 'exec/configure_piecepack --help | sed "s/^/| /" > configurations/configure_piecepack_options.txt'
    sh 'exec/configure_piecepack --help > configurations/configure_piecepack_options.txt'
end

desc "Default piecepack demo"
task :default => :clean
task :default do
    file1 = "configurations/default.json"
    extra_flags = " --file=" + file1
    sh configure_piecepack + extra_flags
    sh "exec/make_piecepack --file=" + file1
    decks = " --decks=deck_filename"
    sh "exec/arrange_piecepacks --collection_filename=default_demo --collection_title='Default demo'" + decks
    sh "xdg-open collections/default_demo.pdf"
end

desc "Orthodox piecepacks demo"
task :orthodox => :clean
task :orthodox do
    deck_title = " --deck_title='Orthodox piecepack'"
    deck_filename = " --deck_filename=orthodox1_piecepack"
    suit_symbols = piecepack_suits + " --suit_colors=darkred,black,darkgreen,darkblue,black"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    file1 = "configurations/orthodox1_piecepack.json"
    extra_flags = " --use_suit_as_ace --file=" + file1 + pyramid_configuration + orthodox_dm + orthodox_saucers
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file1

    deck_title = " --deck_title='Orthodox-style 2-color french-suited piecepack'"
    deck_filename = " --deck_filename=orthodox2_french"
    suit_symbols = french_suits + " --suit_colors=darkred,black,black,darkred,black "
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    file2 = "configurations/orthodox2_french.json"
    extra_flags = " --use_suit_as_ace --file=" + file2 + pyramid_configuration + orthodox_dm + orthodox_saucers
    sh configure_piecepack + deck_title + deck_filename + suit_symbols + rank_symbols + extra_flags 
    sh "exec/make_piecepack --file=" + file2 

    decks = " --decks=orthodox1_piecepack,orthodox2_french"
    sh "exec/arrange_piecepacks --collection_filename=orthodox_demo --collection_title='Orthodox demo'" + decks
    sh "xdg-open collections/orthodox_demo.pdf"
end

desc "Dual piecepacks demo"
task :dual => :clean
task :dual do
    # --background_color=seashell3")
    # dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue"
    # alt_scheme = " --suit_colors=white,orange2,yellow,purple"

    collection_title = " --collection_title='\"Dual piecepacks\" proof of concept (v0.6)'"
    deck_filename = " --deck_filename=dual1piecepack"
    deck_title = " --deck_title='Piecepack-suited (Un-inverted color scheme)'"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    file1 = "configurations/dual1_uninverted_piecepack.json"
    extra_flags = " --use_suit_as_ace --file=" + file1 + pyramid_configuration
    sh configure_piecepack + deck_title + deck_filename + piecepack_suits + rank_symbols + extra_flags 
    sh "exec/make_piecepack --file=" + file1

    deck_filename = " --deck_filename=dual2latin"
    deck_title = " --deck_title='Latin-suited (Inverted color scheme)'"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    file2 = "configurations/dual2_inverted_latin.json"
    extra_flags = "   --invert_colors.suited --use_suit_as_ace --file=" + file2 + pyramid_configuration
    sh configure_piecepack + deck_title + deck_filename + latin_suits + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file2

    deck_filename = " --deck_filename=dual3french"
    deck_title = " --deck_title='French-suited (Dark color scheme)'"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5' --background_color=white" + hexlines_dark
    file3 = "configurations/dual3_dark_french.json"
    extra_flags = " --file=" + file3
    sh configure_piecepack + deck_title + deck_filename + french_suits + rank_symbols + extra_flags 
    sh "exec/make_piecepack --file=" + file3

    deck_filename = " --deck_filename=dual4french"
    deck_title = " --deck_title='French-suited (Light color scheme)'"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5' --background_color=white" + hexlines_light
    file4 = "configurations/dual4_light_french.json"
    extra_flags = " --file=" + file4 + light_scheme
    sh configure_piecepack + deck_title + deck_filename + french_suits + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file4

    deck_filename = " --deck_filename=dual5swiss"
    deck_title = " --deck_title='Swiss-suited (Black color scheme)'"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    black_scheme = " --suit_colors=black,black,black,black,grey40 --background_color=grey70"
    file5 = "configurations/dual5_black_swiss.json"
    extra_flags = " --invert_colors.suited --use_suit_as_ace --file=" + file5 + black_scheme
    sh configure_piecepack + deck_title + deck_filename + swiss_suits + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file5

    deck_filename = " --deck_filename=dual6swiss"
    deck_title = " --deck_title='Swiss-suited (White color scheme)'"
    white_scheme = " --suit_colors=white,white,white,white,grey40 --background_color=grey70"
    file6 = "configurations/dual6_white_swiss.json"
    extra_flags = " --invert_colors.suited --use_suit_as_ace  --file=" + file6 + white_scheme
    sh configure_piecepack + deck_title + deck_filename + swiss_suits + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file6

    decks = " --decks=dual1piecepack,dual2latin,dual3french,dual4french,dual5swiss,dual6swiss"
    sh "exec/arrange_piecepacks --collection_filename=dual_demo" + collection_title + decks
    sh "xdg-open collections/dual_demo.pdf"
end

desc "Test"
task :test => :clean
task :test => :install
task :test do

    # dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue,grey"
    # alt_scheme = " --suit_colors=white,orange2,yellow,purple,grey"

    deck_filename = " --deck_filename=test1piecepack"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    extra_flags = " --use_suit_as_ace  --rank_symbols.chip_face='A,B,C,D,E,F'"
    sh configure_piecepack + deck_filename + piecepack_suits + rank_symbols + extra_flags + ' | exec/make_piecepack'

    # deck_name = " --deck_name='TLD Piecepack, Elements Suits (v0.1)'"
    # deck_filename  = " --deck_filename=elements"
    # suit_symbols = " --suit_symbols=🔥,🌪️,⛰️,🌊,"
      
    deck_filename = " --deck_filename=test2latin"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = "  --invert_colors.suited  "
    sh configure_piecepack + deck_filename + latin_suits + rank_symbols + extra_flags + ' | exec/make_piecepack'

    deck_filename = " --deck_filename=test3french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = " --directional_mark_symbols=,,,, --directional_mark_symbols.tile_face=♥,♠,♣,♦,★ --background_color.unsuited=orange"
    sh configure_piecepack + deck_filename + suit_symbols + rank_symbols + extra_flags + ' | exec/make_piecepack'

    deck_filename = " --deck_filename=test4french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = light_scheme + " --style=simple_hex --invert_colors.unsuited --font='Deja Vu Sans'"
    sh 'exec/configure_piecepack' + deck_filename + suit_symbols + rank_symbols + extra_flags + ' | exec/make_piecepack'

    decks = " --decks=test1piecepack,test2latin,test3french,test4french"
    sh "exec/arrange_piecepacks --collection_filename=test --collection_title='Test'" + decks
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

desc "Run all demos"
task :all do
    Rake::Task[:default].invoke()
    Rake::Task[:dual].invoke()
    Rake::Task[:orthodox].invoke()
end

desc "Copy demos over to Dropbox"
task :copy_demos do
    sh "cp collections/default_demo.pdf /home/trevorld/a/sync/Dropbox/Public/piecepack/"
    sh "cp collections/dual_demo.pdf /home/trevorld/a/sync/Dropbox/Public/piecepack/"
    sh "cp collections/orthodox_demo.pdf /home/trevorld/a/sync/Dropbox/Public/piecepack/"
end
