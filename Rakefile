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
    sh 'sudo Rscript -e "devtools::document()"'
    sh 'sudo Rscript -e "devtools::install(quiet=TRUE, upgrade_dependencies=FALSE)"'
end

desc "Arrange"
task :arrange do
    sh 'exec/arrange_piecepack_images'
    sh "xdg-open collections/*pdf"
end

desc "Dual"
task :dual => :clean
task :dual do
    # dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue"
    # alt_scheme = " --suit_colors=white,orange2,yellow,purple"
    set_label = " --set_label=dual_piecepacks_proof_of_concept_printer_friendly"

    # set_name = " --set_name='\"Dual piecepacks\" proof of concept (v0.4)'"
    set_name = " --set_name='\"Dual piecepacks\" proof of concept (v0.4, more printer-friendly)'"
    deck_label = " --deck_label=1piecepack"
    suit_symbols = " --suit_symbols=🌞,🌜,👑,⚜,꩜"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    extra_flags = " --use_suit_as_ace --background=white"
    sh "exec/make_piecepack_images" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags

    deck_label = " --deck_label=2latin"
    suit_symbols = " --suit_symbols=🏆,🗡️,⚕️,𐇛,꩜"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    extra_flags = "   --inverted --use_suit_as_ace --background=white"
    sh "exec/make_piecepack_images" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags

    deck_label = " --deck_label=3french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5' --background=white --add_hex_lines"
    extra_flags = " "
    sh "exec/make_piecepack_images" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags

    light_scheme = " --suit_colors=hotpink2,dimgrey,palegreen,lightblue2,grey"
    deck_label = " --deck_label=4french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5' --background=white --add_hex_lines"
    extra_flags = " " + light_scheme
    sh "exec/make_piecepack_images" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags

    deck_label = " --deck_label=5swiss"
    suit_symbols = " --suit_symbols=🌹,⛊,🌰,🔔,★"
    rank_symbols = " --rank_symbols=' ,꩜,2,3,4,5'"
    black_scheme = " --suit_colors=black,black,black,black,grey40 --background=grey70"
    extra_flags = " --inverted --use_suit_as_ace" + black_scheme
    sh "exec/make_piecepack_images" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags 

    deck_label = " --deck_label=6swiss"
    suit_symbols = " --suit_symbols=🌹,⛊,🌰,🔔,★"
    white_scheme = " --suit_colors=white,white,white,white,grey40 --background=grey70"
    extra_flags = " --inverted --use_suit_as_ace" + white_scheme
    sh "exec/make_piecepack_images" + set_name + set_label + deck_label + suit_symbols + rank_symbols + extra_flags

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
    extra_flags = " --use_suit_as_ace --suit_colors=darkred,black,darkgreen,darkblue,black --chip_rank_symbols='A,B,C,D,E,F'"
    sh "exec/make_piecepack_images" + set_name + deck_label + suit_symbols + rank_symbols + extra_flags

    # set_name = " --set_name='TLD Piecepack, Elements Suits (v0.1)'"
    # deck_label  = " --deck_label=elements"
    # suit_symbols = " --suit_symbols=🔥,🌪️,⛰️,🌊,"
      
    deck_label = " --deck_label=2latin"
    suit_symbols = " --suit_symbols=🏆,🗡️,⚕️,𐇛,꩜"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = "  --inverted"
    sh "exec/make_piecepack_images" + set_name + deck_label + suit_symbols + rank_symbols + extra_flags

    deck_label = " --deck_label=3french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = " "
    sh "exec/make_piecepack_images" + set_name + deck_label + suit_symbols + rank_symbols + extra_flags

    light_scheme = " --suit_colors=hotpink2,dimgrey,darkolivegreen3,lightblue2,grey"
    deck_label = " --deck_label=4french"
    suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = light_scheme + " --add_hex_lines"
    sh "exec/make_piecepack_images" + set_name + deck_label + suit_symbols + rank_symbols + extra_flags

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

