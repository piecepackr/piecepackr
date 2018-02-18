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
    sh 'sudo Rscript -e "devtools::install()"'
end

desc "Arrange"
task :arrange do
    sh 'exec/arrange_piecepack_images'
end

desc "Test"
task :test => :clean
task :test => :install
task :test do
    set_name = " --set_name='TLD Standardish Piecepack, Piecepack Suits (v1.0)'"
    suit_family = " --suit_family=piecepack"
    suit_symbols = " --suit_symbols=🌜🌞👑⚜"
    rank_symbols = " --rank_symbols=' ꩜2345'"

    sh "exec/make_piecepack_images" + set_name + suit_family +
        suit_symbols + rank_symbols + " --standardish"

    Rake::Task[:arrange].invoke()
end
