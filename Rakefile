desc "Clean"
task :clean do
    rm_r "collections"
    mkdir "collections"
    rm_r "previews"
    mkdir "previews"
    rm_r "png"
    mkdir "png"
    rm_r "pdf"
    mkdir "pdf"
end

desc "Install"
task :install do
    sh 'sudo Rscript -e "devtools::document()"'
    sh 'sudo Rscript -e "devtools::install(quiet=TRUE)"'
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

    sh "killall evince | true"
    set_name = " --set_name='\"Dual piecepacks\" proof of concept (v0.4)'"
    suit_family = " --suit_family=1piecepack"
    suit_symbols = " --suit_symbols=🌜,🌞,👑,⚜"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    # extra_flags = " --standardish"
    extra_flags = " --joker_symbol=꩜ --standardish --background=tan"
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags

    # set_name = " --set_name='TLD Standardish Piecepack, Elements Suits (v0.1)'"
    # suit_family  = " --suit_family=elements"
    # suit_symbols = " --suit_symbols=🌪️ ,🔥,⛰️,🌊"
    # 
    suit_family = " --suit_family=2latin"
    suit_symbols = " --suit_symbols=🗡️,🏆,⚕️,𐇛"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    extra_flags = "  --joker_symbol=꩜ --inverted --standardish --background=tan"
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags

    suit_family = " --suit_family=3french"
    suit_symbols = " --suit_symbols=♠,♥,♣,♦"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5' --background=seashell3"
    extra_flags = " "
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags

    light_scheme = " --suit_colors=dimgrey,hotpink2,palegreen,lightblue2"
    suit_family = " --suit_family=4french"
    suit_symbols = " --suit_symbols=♠,♥,♣,♦"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5' --background=seashell3"
    extra_flags = " " + light_scheme
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags

    suit_family = " --suit_family=5swiss"
    suit_symbols = " --suit_symbols=⛊,🌹,🌰,🔔"
    rank_symbols = " --rank_symbols=' ,꩜,2,3,4,5'"
    black_scheme = " --suit_colors=black,black,black,black --background=grey70 --neutral_col=grey40"
    extra_flags = " --standardish --inverted"
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags + black_scheme

    suit_family = " --suit_family=6swiss"
    suit_symbols = " --suit_symbols=⛊,🌹,🌰,🔔"
    white_scheme = " --suit_colors=white,white,white,white --background=grey70 --neutral_col=grey40"
    extra_flags = " --standardish --inverted"
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags + white_scheme

    Rake::Task[:arrange].invoke()
     
end

desc "Test"
task :test => :clean
task :test => :install
task :test do

    # opts$joker_symbol <- "꩜" 
    # opts$joker_symbol <- "★"
    # opts$joker_symbol <- "♙"
    # dark_scheme = " --suit_colors=black,darkred,darkgreen,darkblue"
    light_scheme = " --suit_colors=dimgrey,hotpink2,darkolivegreen3,lightblue2"
    # alt_scheme = " --suit_colors=white,orange2,yellow,purple"

    sh "killall evince | true"
    set_name = " --set_name='\"Dual piecepacks\" prototypes'"
    suit_family = " --suit_family=1piecepack"
    suit_symbols = " --suit_symbols=🌜,🌞,👑,⚜"
    rank_symbols = " --rank_symbols=' ,A,2,3,4,5'"
    # extra_flags = " --standardish"
    extra_flags = " --joker_symbol=꩜ --standardish"
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags

    # set_name = " --set_name='TLD Standardish Piecepack, Elements Suits (v0.1)'"
    # suit_family  = " --suit_family=elements"
    # suit_symbols = " --suit_symbols=🌪️ ,🔥,⛰️,🌊"
    # 
    # set_name = " --set_name='TLD Standardish Piecepack, Swiss Suits (v0.1)'"
    # suit_family  = " --suit_family=swiss"
    # suit_symbols = " --suit_symbols=🛡 ,🌹,🌰,🔔"
    # 
    # set_name = " --set_name='TLD Standardish Piecepack, Latin Suits (v0.1)'"
    # suit_family  = " --suit_family=latin"
    # suit_symbols = " --suit_symbols=🗡️,🏆,⚕️,𐇛"
    # 
    suit_family = " --suit_family=2latin"
    suit_symbols = " --suit_symbols=🗡️,🏆,⚕️,𐇛"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = "  --joker_symbol=꩜ --inverted --standardish"
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags

    suit_family = " --suit_family=3french"
    suit_symbols = " --suit_symbols=♠,♥,♣,♦"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = " "
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags

    suit_family = " --suit_family=4french"
    suit_symbols = " --suit_symbols=♠,♥,♣,♦"
    rank_symbols = " --rank_symbols='N,A,2,3,4,5'"
    extra_flags = " " + light_scheme
    sh "exec/make_piecepack_images" + set_name + suit_family + suit_symbols + rank_symbols + extra_flags

    # 
    # set_name = " --set_name='TLD Euchre Piecepack, French Suits (v0.1)'"
    # suit_family = " --suit_family=french"
    # suit_symbols = " --suit_symbols=♠,♥,♣,♦"
    # rank_symbols = " --rank_symbols=9,A,J,Q,K,X"
    # 
    # set_name = " --set_name='TLD Chess Piecepack (v0.1)'"
    # suit_family = " --suit_family=chess"
    # # suit_symbols = " --suit_symbols=♟,♟,♙,♙"
    # suit_symbols = " --suit_symbols=♙,♙,♙,♙"
    # # ♞ ♟ ♝ ♜ ♛ ♚ ♘ ♙ ♗ ♖ ♕ ♔
    # rank_symbols = " --rank_symbols=♞,♟,♝,♜,♛,♚"
    # rank_symbols = " --rank_symbols=♘,♙,♗,♖,♕,♔"
    # # add_checkers = TRUE
    # 
    # set_name = " --set_name='TLD Alchemical Piecepack, Elemental Suits (v0.1)'"
    # suit_family = " --suit_family=alchemical"
    # suit_symbols = " --suit_symbols=🜁,🜂,🜃,🜄"
    # # rank_symbols = "  --rank_symbols=' ,𝍩,𝍪,𝍫,𝍬,𝍭'"
    # # rank_symbols = " --rank_symbols=' ,🝔,:,∴,⸪,⁙"
    # # rank_symbols = " --rank_symbols=' 🝔,🜩,☰,⚏,☵"
    # rank_symbols = " --rank_symbols=' 🝪,🜩,ʒ,♃,🜪"
    #
    Rake::Task[:arrange].invoke()

end

