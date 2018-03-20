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

desc "Install"
task :install do
    sh 'sudo Rscript -e "suppressMessages(devtools::document())"'
    sh 'sudo Rscript -e "devtools::install(quiet=TRUE, upgrade_dependencies=FALSE)"'
end

desc "Arrange"
task :arrange do
    sh 'exec/arrange_piecepacks'
    sh "xdg-open collections/*pdf"
end

desc "Dual"
task :dual => :clean
task :dual do
    # --background_color=seashell3")
    # dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue"
    # alt_scheme = " --suit_colors=white,orange2,yellow,purple"
    set_label = " --set_label=dual_piecepacks_proof_of_concept_printer_friendly"

    set_name = " --set_name='\"Dual piecepacks\" proof of concept (v0.5)'"
    deck_label = " --deck_label=1piecepack"
    suit_symbols = " --suit_symbols=🌞,🌜,👑,⚜,꩜"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    file1 = "configurations/dual1_uninverted_piecepack.json"
    extra_flags = " --use_suit_as_ace --background_color=white --file=" + file1
    sh "exec/configure_piecepack" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags 
    sh "exec/make_piecepack --file=" + file1

    deck_label = " --deck_label=2latin"
    suit_symbols = " --suit_symbols=🏆,🗡️,⚕️,𐇛,꩜"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    file2 = "configurations/dual2_inverted_latin.json"
    extra_flags = "   --invert_colors.suited --use_suit_as_ace --background_color=white --file=" + file2
    sh "exec/configure_piecepack" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file2

    deck_label = " --deck_label=3french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5' --background_color=white --style=simple_hex"
    file3 = "configurations/dual3_dark_french.json"
    extra_flags = " --file=" + file3
    sh "exec/configure_piecepack" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags 
    sh "exec/make_piecepack --file=" + file3

    light_scheme = " --suit_colors=hotpink2,dimgrey,palegreen,lightblue2,grey"
    deck_label = " --deck_label=4french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5' --background_color=white --style=simple_hex"
    file4 = "configurations/dual4_light_french.json"
    extra_flags = " --file=" + file4 + light_scheme
    sh "exec/configure_piecepack" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file4

    deck_label = " --deck_label=5swiss"
    suit_symbols = " --suit_symbols=🌹,⛊,🌰,🔔,★"
    rank_symbols = " --rank_symbols=' ,꩜,2,3,4,5'"
    black_scheme = " --suit_colors=black,black,black,black,grey40 --background_color=grey70"
    file5 = "configurations/dual5_black_swiss.json"
    extra_flags = " --invert_colors.suited --directional_marker=matching --use_suit_as_ace --file=" + file5 + black_scheme
    sh "exec/configure_piecepack" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file5

    deck_label = " --deck_label=6swiss"
    suit_symbols = " --suit_symbols=🌹,⛊,🌰,🔔,★"
    white_scheme = " --suit_colors=white,white,white,white,grey40 --background_color=grey70"
    file6 = "configurations/dual6_white_swiss.json"
    extra_flags = " --invert_colors.suited --directional_marker=matching --use_suit_as_ace --file=" + file6 + white_scheme
    sh "exec/configure_piecepack" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags
    sh "exec/make_piecepack --file=" + file6

    Rake::Task[:arrange].invoke()
end

desc "Test"
task :test => :clean
task :test => :install
task :test do

    # dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue,grey"
    # alt_scheme = " --suit_colors=white,orange2,yellow,purple,grey"

    set_name = " --set_name='\"Dual piecepacks\" prototypes'"
    deck_label = " --deck_label=1piecepack"
    suit_symbols = " --suit_symbols=🌞,🌜,👑,⚜,꩜"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    extra_flags = " --use_suit_as_ace --suit_colors=darkred,black,darkgreen,darkblue,black --rank_symbols.chip_face='A,B,C,D,E,F'"
    sh "exec/configure_piecepack" + set_name + deck_label + suit_symbols + rank_symbols + extra_flags + ' | exec/make_piecepack'

    # set_name = " --set_name='TLD Piecepack, Elements Suits (v0.1)'"
    # deck_label  = " --deck_label=elements"
    # suit_symbols = " --suit_symbols=🔥,🌪️,⛰️,🌊,"
      
    deck_label = " --deck_label=2latin"
    suit_symbols = " --suit_symbols=🏆,🗡️,⚕️,𐇛,꩜"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = "  --invert_colors.suited --directional_marker=matching "
    sh "exec/configure_piecepack" + set_name + deck_label + suit_symbols + rank_symbols + extra_flags + ' | exec/make_piecepack'

    deck_label = " --deck_label=3french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = " --directional_marker=none --background_color.unsuited=orange"
    sh "exec/configure_piecepack" + set_name + deck_label + suit_symbols + rank_symbols + extra_flags + ' | exec/make_piecepack'

    light_scheme = " --suit_colors=hotpink2,dimgrey,darkolivegreen3,lightblue2,grey"
    deck_label = " --deck_label=4french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = light_scheme + " --style=simple_hex --directional_marker=matching --invert_colors.unsuited"
    sh "exec/configure_piecepack" + set_name + deck_label + suit_symbols + rank_symbols + extra_flags + ' | exec/make_piecepack'

    # set_name = " --set_name='TLD Euchre Piecepack, French Suits (v0.1)'"
    # deck_label = " --deck_label=french"
    # suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    # rank_symbols = " --rank_symbols=9,10,J,Q,K,A"
      
    # set_name = " --set_name='TLD Chess Piecepack (v0.1)'"
    # deck_label = " --deck_label=chess"
    # # suit_symbols = " --suit_symbols=♟,♟,♙,♙,"
    # suit_symbols = " --suit_symbols=♙,♙,♙,♙,♙"
    # rank_symbols = " --rank_symbols=♞,♟,♝,♜,♛,♚"
    # rank_symbols = " --rank_symbols=♘,♙,♗,♖,♕,♔"
    # # add_checkers = TRUE
      
    # set_name = " --set_name='TLD Alchemical Piecepack, Elemental Suits (v0.1)'"
    # deck_label = " --deck_label=alchemical"
    # suit_symbols = " --suit_symbols=🜁,🜂,🜃,🜄,"
    # # rank_symbols = "  --rank_symbols=' ,𝍩,𝍪,𝍫,𝍬,𝍭'"
    # # rank_symbols = " --rank_symbols=' ,🝔,:,∴,⸪,⁙"
    # # rank_symbols = " --rank_symbols=' 🝔,🜩,☰,⚏,☵"
    # rank_symbols = " --rank_symbols=' 🝪,🜩,ʒ,♃,🜪"
    Rake::Task[:arrange].invoke()
end

desc "Run all demos"
task :all do
    Rake::Task[:dual].invoke()
end

