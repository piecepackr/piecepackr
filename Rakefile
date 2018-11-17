# encoding: utf-8

desc "Clean"
task :clean do
    dirs = ["bmp", "jpeg", "pdf", "png", "ps", "svg", "tiff"]
    dirs.each { |dir|
        sh "rm -r #{dir} | true"
    }
    sh "killall evince | true"
end

def version
    version = 'v' + `Rscript -e "cat(packageDescription('piecepackr')[['Version']])"`
    return version
end

def version_str
    version_str = ' (' + version + ')"'
    return version_str
end

desc "Print version"
task :version do
    puts version
end 

collect_piecepacks = 'Rscript exec/collect_pnp --author="Trevor L Davis" '

#### cat one.json two.json | jq -s add

def make_piecepack (filename, extra_flags, n_suits=0, n_ranks=0)
    make_images = ENV.has_key?("make_images") # rake all make_images=
    configure_piecepack = "PP_N_RANKS=#{n_ranks} PP_N_SUITS=#{n_suits} Rscript exec/configure "
    json_file = "configurations/" + filename + ".json"
    sh configure_piecepack + " --output_file=" + json_file + extra_flags
    sh "Rscript exec/make_pnp --cfg_file=" + json_file + " --deck_name=" + filename 
    sh "Rscript exec/make_preview --cfg_file=" + json_file + " --deck_name=" + filename 
    if make_images
        sh "Rscript exec/make_images --cfg_file=" + json_file + " --deck_name=" + filename 
    end
    return nil
end

# Colors
chinese_elements_colors = " --suit_colors=darkgreen,red3,chocolate4,black,darkblue,grey"
# alt = "white,orange2,yellow,purple"
# light = "hotpink2,dimgrey,palegreen,lightblue2,"
dark = "darkred,black,darkgreen,darkblue,"
# light_scheme = light + "grey"
# http://jfly.iam.u-tokyo.ac.jp/color/#see
# Black #000000
# Orange #E69F00
# Sky Blue #56B4E9
# Bluish Green #009E73
# Yellow #F0E442
# Blue #0072B2
# Vermillion #D55E00
# Reddish Purple #F079A7
colorblind_friendly = " --suit_colors=#D55E00,#000000,#009E73,#56B4E9,#E69F00"
dark_scheme = dark + "grey"
hexlines_dark = " --hexline_colors.tile_face=" + dark_scheme 
# hexlines_light = " --hexline_colors.tile_face=" + light_scheme 
rd_dark = "#ff0000,#ffc000,#008000,#0000ff,#800080,#000000,sienna"
rd_light = "#ff80c0,#ffff80,#80ff00,#80ffff,#c080ff,#c0c0c0,sienna"
rd_dark2 = "#ff0000,#ffc000,#008000,#0000ff,#800080,#000000,white"
rd_light2 = "#ff80c0,#ffff80,#80ff00,#80ffff,#c080ff,#c0c0c0,white"

# Suits
jcd_suits = ' --suit_symbols=🌞,🌜,♚,⚓,꩜ --suit_symbols_scale=0.6,0.7,1,0.8,0.9 --suit_symbols_font="Noto Emoji,Noto Sans Symbols2,Noto Sans Symbols,DejaVu Sans,Noto Sans Cham"'
all_french_suits = ' --suit_symbols=♥,♠,♣,♦,★,★,꩜ --suit_symbols_scale=1,1,1,1,1,1,0.9 --suit_symbols_font="Noto Sans Symbols"'
# latin_suits_swirl = ' --suit_symbols=🏆,🗡️,⚕️,𐇛,꩜ --suit_symbols_scale=0.6,1,1,1.1,1'
latin_suits_swirl = ' --suit_symbols=,,,,꩜ --suit_symbols_font=Quivira --suit_symbols_scale=0.9,1,1,1,0.9'
piecepack_suits = ' --suit_symbols=🌞,🌜,👑,⚜,꩜ --suit_symbols_scale=0.6,0.7,0.75,0.9,0.9 --suit_symbols_font="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham"'
all_piecepack_suits = ' --suit_symbols=🌞,🌜,👑,⚜,♚,⚓,꩜ --suit_symbols_scale=0.6,0.7,0.75,0.9,1,0.8,0.9 --suit_symbols_font="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Symbols,Noto Sans Symbols,Noto Sans Cham"'
rd_suits = ' --suit_symbols=♥,★,♣,♦,♛,♠,꩜ --suit_symbols_scale=1,1,1,1,0.85,1,0.9 --suit_symbols_font="Noto Sans Symbols"'
sixpack_suits = ' --suit_symbols=♥,♠,♣,♦,🌞,🌜,꩜ --suit_symbols_scale=0.7,0.7,0.7,0.7,0.6,0.6,0.9 --suit_symbols_font="Noto Emoji"'
sixpack_suits_black = ' --suit_symbols=♥,♠,♣,♦,☀,🌘,꩜ --suit_symbols_scale=1.2,1.2,1.2,1.2,1.2,0.7,0.9'
sixpack_suits_white = ' --suit_symbols=♡,♤,♧,♢,☼,🌔,꩜ --suit_symbols_scale=1,1,1,1,1.4,0.7,0.9'
black_suits4 = ' --suit_symbols=♠,♣,☽,⚓,★  --suit_symbols_font="Noto Sans Symbols,Noto Sans Symbols,Noto Sans Symbols,DejaVu Sans,Noto Sans Symbols"'
coin_black_suits4 = ' --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1 --suit_symbols_font.coin_back="Noto Sans Symbols" --dm_symbols.coin_back=♠,♣,☽,⚓,★  --dm_symbols_font.coin_back="Noto Sans Symbols,Noto Sans Symbols,Noto Sans Symbols2,DejaVu Sans,Noto Sans Symbols"'
black_suits6 = ' --suit_symbols=♠,♣,🌜,⚓,,,★  --suit_symbols_font="Noto Sans Symbols,Noto Sans Symbols,Noto Sans Symbols2,DejaVu Sans,Quivira,Quivira,Noto Sans Symbols" --suit_symbols_scale=1,1,0.8,0.8,1,1,1'
coin_black_suits6 = ' --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1 --suit_symbols_font.coin_back="Noto Sans Symbols" --dm_symbols.coin_back=♠,♣,🌜,⚓,,,★  --dm_symbols_font.coin_back="Noto Sans Symbols,Noto Sans Symbols,Noto Sans Symbols2,DejaVu Sans,Quivira,Quivira,Noto Sans Symbols" --dm_symbols_scale.coin_back=1,1,0.8,1,1,1,1'
red_suits4 = ' --suit_symbols=♥,♦,☉,♚,★  --suit_symbols_font="Noto Sans Symbols"'
coin_red_suits4 = ' --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1 --suit_symbols_font.coin_back="Noto Sans Symbols" --dm_symbols.coin_back=♥,♦,☉,♚,★  --dm_symbols_font.coin_back="Noto Sans Symbols"'
red_suits6 = ' --suit_symbols=♥,♦,🌞,♚,,,★  --suit_symbols_font="Noto Sans Symbols,Noto Sans Symbols,Noto Emoji,Noto Sans Symbols,Quivira,Quivira,Noto Sans Symbols" --suit_symbols_scale=1,1,0.8,1,1,1,1'
coin_red_suits6 = ' --dm_symbols.coin_face=★ --suit_symbols.coin_back=♟ --suit_symbols_scale.coin_back=1 --suit_symbols_font.coin_back="Noto Sans Symbols" --dm_symbols.coin_back=♥,♦,🌞,♚,,,★  --dm_symbols_font.coin_back="Noto Sans Symbols,Noto Sans Symbols,Noto Emoji,Noto Sans Symbols,Quivira,Quivira,Noto Sans Symbols" --dm_symbols_scale.coin_back=1,1,0.8,1,1,1,1'
alchemical_elements = " --suit_symbols=🜁,🜂,🜃,🜄, --suit_colors=red,khaki2,green,cornflowerblue,grey"
# emoji_elements = " --suit_symbols=🔥,🌪️,⛰️,🌊, --suit_colors=darkred,black,darkgreen,darkblue"
chinese_elements_emoji = ' --suit_symbols=🌲,🔥,⛰️,🏆,🌊, --suit_symbols_scale=0.6,0.5,0.6,0.6,0.5 --suit_symbols_font="Noto Emoji,Noto Emoji,Noto Emoji,Noto Sans Symbols2,Noto Emoji"' + chinese_elements_colors
chinese_elements_sc = ' --suit_symbols=木,火,土,金,水, --suit_symbols_scale=0.7 --suit_symbols_font="Noto Sans CJK SC"' + chinese_elements_colors 

# Ranks
default_ranks = ' --rank_symbols=n,a,2,3,4,5'
null_ace_ranks = ' --rank_symbols=∅,A,2,3,4,5'
default_ranks_noto = default_ranks + ' --rank_symbols_font="Noto Sans"'
orthodox_ranks = ' --rank_symbols=,A,2,3,4,5 --use_suit_as_ace'
orthodox_ranks_noto = orthodox_ranks + ' --rank_symbols_font="Noto Sans"'
chinese_ranks_emoji1 = ' --rank_symbols=🐀,🐉,🐒,🐂,🐍,🐓 --rank_symbols_scale=0.6 --rank_symbols_font="Noto Emoji"'
chinese_ranks_emoji2 = ' --rank_symbols=🐅,🐎,🐕,🐇,🐏,🐖 --rank_symbols_scale=0.6 --rank_symbols_font="Noto Emoji"'
chinese_ranks_sc1 = ' --rank_symbols=鼠,龙,猴,牛,蛇,鸡 --rank_symbols_font="Noto Sans CJK SC" --rank_symbols_scale=0.7'
chinese_ranks_sc2 = ' --rank_symbols=虎,马,狗,兔,羊,猪 --rank_symbols_font="Noto Sans CJK SC" --rank_symbols_scale=0.7'

# Configurations
dozenal_chips = " --shape.chip=5"
pyramid_configuration = " --rank_symbols.chip_face='A,B,C,D,E,F' --use_ace_as_ace.chip_face --dm_symbols.chip= --shape.chip=kite"
orthodox_dm = ' --dm_colors.coin=black --dm_symbols.ppdie= --dm_symbols.coin="|" --dm_symbols_font.coin="Noto Sans" --dm_r.coin=0.45'
# ○ ⚆ ⚇ ● ⚈ ⚉
orthodox_saucers = ' --suit_symbols.saucer_back= --dm_colors.saucer=black --dm_symbols.saucer="|" --dm_symbols_font.saucer="Noto Sans" --dm_r.saucer=0.45'
orthodox_pawns = " --invert_colors.pawn --suit_symbols.pawn= --dm_symbols.pawn="
orthodox_pawns6p = " --invert_colors.pawn --suit_symbols.pawn=⚈,⚈,⚉,⚉,, --dm_symbols.pawn="

orthodox_saucers1 = ' --suit_symbols.saucer_face=● --suit_symbols_scale.saucer_face=0.7 --suit_symbols_font.saucer_face="Noto Sans Symbols"' + orthodox_saucers
orthodox_saucers2 = ' --suit_symbols.saucer_face=⚈,⚉,⚈,⚉, --suit_symbols_scale.saucer_face=0.7 --suit_symbols_font.saucer_face="Noto Sans Symbols"' + orthodox_saucers
orthodox_saucers3 = ' --suit_symbols.saucer_face=⚈,⚈,⚉,⚉,●,●, --suit_symbols_scale.saucer_face=0.7,0.7,0.7,0.7,0.7,0.7 --suit_symbols_font.saucer_face="Noto Sans Symbols"' + orthodox_saucers
hex_components = " --shape.tile=6 --shape.coin=3 --shape.saucer=3 --shape.chip=3 --dm_symbols_scale.chip=0.7 --shape_theta.coin=-90 --shape_theta.saucer=-90 --shape_theta.chip=-90 --dm_theta.coin=-90 --dm_theta.saucer=-90 --dm_theta.chip=-90"
star_chips  = " --shape.chip=star --rank_symbols_scale.chip_face=0.7 --suit_symbols_scale.chip_back=0.7 --dm_symbols_scale.chip=0.7"

## Demos
demos = ["chess", "chinese_zodiac", "crown_and_anchor", "default",
         "dual", "hex", "orthodox", "rainbow_deck", "reversi", "sixpack", "yellow_crown"]

desc "Run all demos"
multitask :all => demos

desc "Chess demo"
task :chess do
    deck_title = ' --deck_title="Piecepack-suits' + version_str
    file = "chess1"
    extra_flags = " --checker_colors.tile_face=grey --suit_colors=darkred,black,dimgrey,hotpink2,grey"
    internal_cfgs = " --internal_cfgs=chess_ranks"
    make_piecepack file, deck_title + piecepack_suits + " --suit_colors=darkred,black,darkgreen,darkblue,black"

    deck_title = ' --deck_title="French-suits (checkered tile faces)' + version_str
    file = "chess2"
    extra_flags = " --checker_colors.tile_face=grey --suit_colors=darkred,black,dimgrey,hotpink2,grey"
    internal_cfgs = " --internal_cfgs=chess_ranks,french_suits_noto,swirl_s5_noto"
    make_piecepack file, deck_title + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Black-suits (checkered tile backs)' + version_str
    file = "chess3"
    extra_flags = " --checker_colors.tile_back=darkred --gridline_colors=  --suit_colors=black,black,black,black,darkred" + coin_black_suits4
    internal_cfgs = " --internal_cfgs=chess_ranks"
    make_piecepack file, deck_title + black_suits4 + + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Red-suits (checkered tile backs)' + version_str
    file = "chess4"
    extra_flags = " --checker_colors.tile_back=black --gridline_colors= --suit_colors=darkred,darkred,darkred,darkred,black" + coin_red_suits4
    internal_cfgs = " --internal_cfgs=chess_ranks"
    make_piecepack file, deck_title + red_suits4 + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Black-suits (checkered tile faces)' + version_str
    file = "chess5"
    extra_flags = " --checker_colors.tile_face=grey --suit_colors=black,black,black,black,black,black,darkred" + coin_black_suits6
    internal_cfgs = " --internal_cfgs=chess_ranks"
    make_piecepack file, deck_title + black_suits6 + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Red-suits (checkered tile faces)' + version_str
    file = "chess6"
    extra_flags = " --checker_colors.tile_face=grey --suit_colors=darkred,darkred,darkred,darkred,darkred,darkred,black " + coin_red_suits6
    internal_cfgs = " --internal_cfgs=chess_ranks"
    make_piecepack file, deck_title + red_suits6 + extra_flags + internal_cfgs

    decks = " --deck_names=chess1,chess2,chess3,chess4,chess5,chess6"
    sh collect_piecepacks + ' --collection_name=chess_demo --title="Chess ranked piecepack demo' + version_str + decks

end

desc "Chinese zodiac demo"
task :chinese_zodiac do
    file = "chinese_zodiac1"
    deck_title = ' --deck_title="Chinese zodiac #1' + version_str
    make_piecepack file, deck_title + chinese_elements_emoji + chinese_ranks_emoji1 + dozenal_chips

    deck_title = ' --deck_title="Chinese zodiac #2' + version_str
    file = "chinese_zodiac2"
    make_piecepack file, deck_title + chinese_elements_emoji + chinese_ranks_emoji2 + dozenal_chips

    deck_title = ' --deck_title="Chinese zodiac #3' + version_str
    file = "chinese_zodiac3"
    make_piecepack file, deck_title + chinese_elements_sc + chinese_ranks_sc1 + dozenal_chips

    deck_title = ' --deck_title="Chinese zodiac #4' + version_str
    file = "chinese_zodiac4"
    make_piecepack file, deck_title + chinese_elements_sc + chinese_ranks_sc2 + dozenal_chips

    decks = " --deck_names=chinese_zodiac1,chinese_zodiac2,chinese_zodiac3,chinese_zodiac4"
    sh collect_piecepacks + ' --collection_name=chinese_zodiac_demo --title="Chinese zodiac demo' + version_str + decks
end


desc "Crown and anchor demo"
task :crown_and_anchor do

    deck_title = ' --deck_title="Crown and anchor suits' + version_str
    file = "crown_and_anchor1"
    extra_flags = " --suit_colors=darkred,darkred,black,black,darkred,black,grey"
    internal_cfgs = " --internal_cfgs=ca_suits_noto"
    make_piecepack file,  deck_title + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Crown and anchor suits (Multicolor)' + version_str
    file = "crown_and_anchor2"
    extra_flags = " --suit_colors=darkred,darkblue,darkgreen,black,purple,orange2,grey"
    internal_cfgs = " --internal_cfgs=ca_suits_noto"
    make_piecepack file, deck_title + default_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="JCD piecepack suits (Monoscale)' + version_str
    file = "crown_and_anchor3"
    extra_flags = " --suit_colors=black,white,black,white,black --background_colors=white,black,white,black,white"
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + jcd_suits + default_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="JCD piecepack suits (Black/Red)' + version_str
    file = "crown_and_anchor4"
    extra_flags = " --suit_colors=darkred,black,darkred,black,grey"
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + jcd_suits + default_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="JCD piecepack suits (Multicolor)' + version_str
    file = "crown_and_anchor5"
    extra_flags = " --suit_colors=darkred,black,darkgreen,darkblue,grey"
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + jcd_suits + default_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="JCD piecepack suits (Alternative Multicolor)' + version_str
    file = "crown_and_anchor6"
    extra_flags = " --suit_colors=yellow,white,purple,orange2,tan4 --background_colors=seashell3"
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + jcd_suits + default_ranks_noto + extra_flags+ internal_cfgs

    title = ' --title="Crown and anchor demo' + version_str
    decks = " --deck_names=crown_and_anchor1,crown_and_anchor2,crown_and_anchor3,crown_and_anchor4,crown_and_anchor5,crown_and_anchor6"
    sh collect_piecepacks + " --collection_name=crown_and_anchor_demo" + title + decks
end

desc "Default piecepack demo"
task :default do
    file = "default"
    extra_flags = '' + colorblind_friendly
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, extra_flags+ internal_cfgs

    decks = " --deck_names=default"
    title = ' --title="Default piecepack deck demo' + version_str
    sh collect_piecepacks + " --collection_name=default_demo" + title + decks
end

desc "Dual piecepacks demo"
task :dual do

    deck_title = ' --deck_title="Piecepack-suited (Un-inverted color scheme)' + version_str
    file = "dual1"
    extra_flags = " --suit_colors=" + dark_scheme
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + piecepack_suits + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Latin-suited (Inverted color scheme)' + version_str
    file = "dual2"
    extra_flags = "  --background_colors=white --invert_colors.suited --suit_colors=" + dark_scheme
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + latin_suits_swirl + default_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="French-suited (Dark color scheme)' + version_str
    file = "dual3"
    extra_flags = hexlines_dark + ' --suit_colors=' + dark_scheme
    internal_cfgs = " --internal_cfgs=french_suits_noto,swirl_s5"
    make_piecepack file, deck_title + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="French-suited (Light color scheme)' + version_str
    file = "dual4"
    # extra_flags = " --suit_colors=" + light_scheme + hexlines_light
    extra_flags = hexlines_dark + ' --suit_colors=' + dark_scheme
    internal_cfgs = " --internal_cfgs=french_suits_white_noto,swirl_s5"
    make_piecepack file, deck_title + default_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="Swiss-suited (Black color scheme)' + version_str
    black_scheme = " --suit_colors=black,black,black,black,grey40 --background_colors=grey70"
    file = "dual5"
    extra_flags = " --invert_colors.suited --use_suit_as_ace" + black_scheme
    internal_cfgs = " --internal_cfgs=swiss_suits_quivira"
    make_piecepack file, deck_title + orthodox_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="Swiss-suited (White color scheme)' + version_str
    white_scheme = " --suit_colors=white,white,white,white,grey40 --background_colors=grey70"
    file = "dual6"
    extra_flags = " --invert_colors.suited --use_suit_as_ace" + white_scheme
    internal_cfgs = " --internal_cfgs=swiss_suits_quivira"
    make_piecepack file, deck_title + orthodox_ranks_noto + extra_flags+ internal_cfgs

    title = ' --title="Dual piecepacks proof of concept' + version_str
    decks = " --deck_names=dual1,dual2,dual3,dual4,dual5,dual6"
    sh collect_piecepacks + " --collection_name=dual_demo" + title + decks
end

desc "Hex-friendly piecepacks demo"
task :hex do
    deck_title = ' --deck_title="French-suited (Colored hexlines on tile faces)' + version_str
    file = "hex1"
    extra_flags = hexlines_dark + ' --suit_colors=' + dark_scheme
    internal_cfgs = " --internal_cfgs=french_suits_noto,swirl_s5_noto"
    make_piecepack file, deck_title + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="French-suited (Grey hexlines on tile faces/backs)' + version_str
    file = "hex2"
    extra_flags = " --hexline_colors.tile=grey" + ' --suit_colors=' + dark_scheme
    internal_cfgs = " --internal_cfgs=french_suits_noto,swirl_s5_noto"
    make_piecepack file, deck_title + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Piecepack-suited (Hex-shaped tile faces/backs)' + version_str
    file = "hex3" 
    extra_flags = " --dm_theta.tile_face=120" + hex_components + " --background_colors=hotpink2,grey,grey,hotpink2,white" + ' --suit_colors=' + dark_scheme
    internal_cfgs = " --internal_cfgs=french_suits_noto,swirl_s5_noto"
    make_piecepack file, deck_title + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Piecepack-suited (Hexpack-ish style)' + version_str
    file = "hex4"
    extra_flags = " --shape_theta.tile_face=0 --shape_theta.tile_back=0 --dm_theta.tile_face=-90 --gridline_colors= --checker_colors.tile_back=grey --suit_colors.chip_back=" + hex_components + ' --suit_colors=' + dark_scheme
    internal_cfgs = " --internal_cfgs=french_suits_noto,swirl_s5_noto"
    make_piecepack file, deck_title + default_ranks_noto + extra_flags + internal_cfgs

    decks = " --deck_names=hex1,hex2,hex3,hex4"
    sh collect_piecepacks + ' --collection_name=hex_demo --title="Hex-friendly piecepack demo' + version_str + decks
end

desc "Orthodox piecepack demo"
task :orthodox do
    deck_title = ' --deck_title="Orthodox piecepack' + version_str
    suit_symbols = piecepack_suits + " --suit_colors=darkred,black,darkgreen,darkblue,black"
    file = "orthodox1"
    extra_flags = " --use_suit_as_ace" + pyramid_configuration + orthodox_dm + orthodox_saucers1 + orthodox_pawns
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + orthodox_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="Orthodox-style 2-color french-suited piecepack' + version_str
    suit_symbols = " --suit_colors=darkred,black,black,darkred,black "
    file = "orthodox2"
    extra_flags = " --use_suit_as_ace" + pyramid_configuration + orthodox_dm + orthodox_saucers2 + orthodox_pawns
    internal_cfgs = " --internal_cfgs=french_suits_noto,swirl_s5_noto"
    make_piecepack file, deck_title + suit_symbols + orthodox_ranks_noto + extra_flags + internal_cfgs

    decks = " --deck_names=orthodox1,orthodox2"
    sh collect_piecepacks + ' --collection_name=orthodox_demo --title="Orthodox demo' + version_str + decks
end

desc "Yellow crown piecepack demo"
task :yellow_crown do
    deck_title = ' --deck_title="Orthodox piecepack' + version_str
    suit_symbols = piecepack_suits + " --suit_colors=darkred,black,gold,darkblue,black"
    file = "yellow_crown1"
    extra_flags = " --use_suit_as_ace" + pyramid_configuration + orthodox_dm + orthodox_saucers1 + orthodox_pawns
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + orthodox_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="Inverted piecepack' + version_str
    suit_symbols = piecepack_suits + " --suit_colors=darkred,black,gold,darkblue,black"
    file = "yellow_crown2"
    extra_flags = " --invert_colors.suited --uninvert_colors.r1 --invert_colors.s5.r1" 
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + null_ace_ranks + extra_flags + internal_cfgs, 5, 1

    deck_title = ' --deck_title="All piecepack suits' + version_str
    suit_symbols = all_piecepack_suits + " --suit_colors=red,black,green,dodgerblue2,yellow2,darkblue,sienna"
    file = "yellow_crown3"
    extra_flags = " --background_colors=black,white,white,black,black,white,grey"
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags+ internal_cfgs

    deck_title = ' --deck_title="All french suits' + version_str
    suit_symbols = all_french_suits + " --suit_colors=red,black,green,dodgerblue2,yellow2,darkblue,sienna"
    file = "yellow_crown4"
    extra_flags = " --background_colors=black,white,white,black,black,white,grey"
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags+ internal_cfgs

    decks = " --deck_names=yellow_crown1,yellow_crown2,yellow_crown3,yellow_crown4"
    sh collect_piecepacks + ' --collection_name=yellow_crown_demo --title="Yellow Crown demo' + version_str + decks

end

desc "Rainbow deck demo"
multitask :rainbow_deck => [:rd1, :rd2, :rd3, :rd4, :rd5, :rd6]
multitask :rainbow_deck do
    decks = " --deck_names=rainbow_deck1,rainbow_deck2,rainbow_deck3,rainbow_deck4,rainbow_deck5,rainbow_deck6"
    sh collect_piecepacks + ' --collection_name=rainbow_deck_demo --title="Rainbow Deck suited piecepack demo' + version_str + decks + " --subject='The Rainbow Deck (RD) is a cardgame system by Chen Changcai.  More information is available at https://boardgamegeek.com/boardgame/59655/rainbow-deck.'"
end
task :rd1 do
    deck_title = ' --deck_title="RD-suited piecepack (Dark suits)' + version_str
    suit_symbols = rd_suits + ' --suit_colors=' + rd_dark
    file = "rainbow_deck1"
    extra_flags = dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs
end
task :rd2 do
    deck_title = ' --deck_title="RD-suited piecepack (Light suits)' + version_str
    suit_symbols = rd_suits + ' --suit_colors=' + rd_light
    file = "rainbow_deck2"
    extra_flags = dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs
end
rd_background = " --background_colors=sienna"
task :rd3 do
    deck_title = ' --deck_title="RD-suited piecepack (Dark suits)' + version_str
    suit_symbols = rd_suits + ' --suit_colors=' + rd_dark2
    file = "rainbow_deck3"
    extra_flags = "" + rd_background + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs
end
task :rd4 do
    deck_title = ' --deck_title="RD-suited piecepack (Light suits)' + version_str
    suit_symbols = rd_suits + ' --suit_colors=' + rd_light2
    file = "rainbow_deck4"
    extra_flags = "" + rd_background + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs
end
task :rd5 do
    deck_title = ' --deck_title="RD-suited piecepack (Dark suits)' + version_str
    suit_symbols = rd_suits + ' --suit_colors=' + rd_dark
    file = "rainbow_deck5"
    extra_flags = " --background_colors.unsuited=grey" + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs
end
task :rd6 do
    deck_title = ' --deck_title="RD-suited piecepack (Light suits)' + version_str
    suit_symbols = rd_suits + ' --suit_colors=' + rd_light
    file = "rainbow_deck6"
    extra_flags = " --background_colors.suited=black --background_colors.unsuited=grey" + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs
end

desc "Reversi-friendly piecepacks demo"
task :reversi do
    deck_title = ' --deck_title="Piecepack-suited' + version_str
    suit_symbols = piecepack_suits + ' --suit_colors=' + dark_scheme
    file = "reversi1"
    extra_flags = " --background_colors.suited=tan3 --background_colors.chip_back=white"
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Elements-suited' + version_str
    suit_symbols = alchemical_elements
    file = "reversi2"
    extra_flags = " --background_colors.suited=black --background_colors.chip_back=white"
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Sixpack (Black suits)' + version_str
    suit_symbols = sixpack_suits_black + " --suit_colors=black,black,black,black,black,black,grey"
    file = "reversi3"
    extra_flags = " --suit_colors.chip_back=grey" + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Sixpack (White suits)' + version_str
    suit_symbols = sixpack_suits_white + " --suit_colors=black,black,black,black,black,black,grey"
    file = "reversi4"
    extra_flags = " --suit_colors.chip_back=grey" + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Red-suited ' + version_str
    suit_symbols = red_suits6 + " --suit_colors=white --background_colors.suited=darkred --background_colors.unsuited=black"
    file = "reversi5"
    extra_flags = " --background_colors.chip_back=black" + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Black-suited ' + version_str
    suit_symbols = black_suits6 + " --suit_colors=white --background_colors.suited=black --background_colors.unsuited=darkred"
    file = "reversi6"
    extra_flags = " --background_colors.chip_back=darkred" + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs

    decks = " --deck_names=reversi1,reversi2,reversi3,reversi4,reversi5,reversi6"
    sh collect_piecepacks + ' --collection_name=reversi_demo --title="Reversi-friendly piecepacks demo' + version_str + decks
end

desc "Sixpack piecepacks demo"
task :sixpack do
    deck_title = ' --deck_title="Sixpack' + version_str
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,black,darkred,darkred,black,grey'
    file = "sixpack1"
    extra_flags = ""
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Sixpack (Orthodox style)' + version_str
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,black,darkred,darkred,black,black'
    file = "sixpack2"
    extra_flags = "" + pyramid_configuration + orthodox_dm + orthodox_saucers3 + orthodox_pawns6p
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + orthodox_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Sixpack (Dark multicolor scheme)' + version_str
    # sixpack_suits2 = " --suit_symbols=♥,♤,♣,♦,🌞,🌜,꩜ --suit_symbols_scale=1,1,1,1,0.7,0.8,1"
    suit_symbols = sixpack_suits + ' --suit_colors=darkred,black,darkgreen,darkblue,orange3,purple,tan4'
    file = "sixpack3"
    extra_flags = " --background_colors.unsuited=seashell3" + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs

    deck_title = ' --deck_title="Sixpack (Light multicolor scheme)' + version_str
    suit_symbols = sixpack_suits + ' --suit_colors=hotpink2,grey,palegreen,lightblue1,yellow,white,tan4'
    file = "sixpack4"
    extra_flags = " --background_colors.unsuited=seashell3 --background_colors.suited=black" + dozenal_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, deck_title + suit_symbols + default_ranks_noto + extra_flags + internal_cfgs

    decks = " --deck_names=sixpack1,sixpack2,sixpack3,sixpack4"
    sh collect_piecepacks + ' --collection_name=sixpack_demo --title="Sixpack demo' + version_str + decks
end

## Development Utilities
desc "Copy all demos over to Dropbox"
task :copy_demos do
    dir = "/home/trevorld/a/sync/Dropbox/Public/piecepack/"
    demos.each { |x| sh "cp pdf/collections/#{x}_demo.pdf #{dir}" }
end

desc "Update package documentation"
task :document do
    sh 'Rscript -e "suppressMessages(devtools::document())"'
    Rake::Task["install"].invoke
    # sh 'Rscript exec/configure --help | sed "s/^/| /" > configurations/configure_piecepack_options.txt'
    sh 'PP_N_RANKS=6 PP_N_SUITS=5 Rscript exec/configure --help | cat > txt/configure_options.txt | true'
    sh 'Rscript exec/make_piecepack_preview --help | cat > txt/make_preview_options.txt'
    sh 'Rscript exec/make_piecepack_images --help | cat > txt/make_images_options.txt'
    sh 'Rscript exec/make_pnp_piecepack --help | cat > txt/make_pnp_options.txt'
    sh 'Rscript exec/collect_pnp_piecepacks --help | cat > txt/collect_pnp_options.txt'
    sh 'Rscript exec/get_embedded_font --help | cat > txt/get_embedded_font_options.txt'
end

desc "(Re-)install piecepack R package"
task :install do
    use_sudo = ENV.has_key?("sudo") # rake document sudo=
    is_quiet = ENV.has_key?("quiet") # rake document sudo= quiet=
    if is_quiet
        cmd = 'Rscript -e "devtools::install(quiet=TRUE, dependencies=c(\'Imports\', \'Suggests\'), upgrade_dependencies=FALSE)"'
    else
        cmd = 'Rscript -e "devtools::install(dependencies=c(\'Imports\', \'Suggests\'), upgrade_dependencies=FALSE)"'
    end
    if use_sudo
        cmd = 'sudo ' + cmd
    end
    sh cmd
end

desc "Upload codecov coverage report"
task :codecov do
    sh 'CODECOV_TOKEN="2db4a29a-971e-45e5-a230-676b5289d801" Rscript -e "covr::codecov()"'
end

desc "Install R package and dependencies on Ubuntu (16.04+) (use sudo= to use sudo and yes= to use -y)"
task :apt_install_dependencies do
    use_sudo ENV.has_key?("sudo") # rake install_dependencies_apt sudo=
    use_yes ENV.has_key?("yes") # rake install_dependencies_apt sudo= yes=
    if use_yes
        apt_install = 'apt install -y '
    else
        apt_install = 'apt install '
    end
    if use_sudo
        apt_install = 'sudo ' + apt_install
        rscript = 'sudo Rscript '
    else
        rscript = 'Rscript '
    end
    sh apt_install + 'ghostscript poppler-utils r-base'
    sh apt_install + 'libcurl4-openssl-dev libssl-dev libxml2-dev libcairo2-dev'
    sh rscript + '-e "install.packages(\'devtools\', repos=\'https://cran.rstudio.com/\')"'
    sh apt_install + 'fonts-dejavu fonts-noto rake'
    fonts_dir = ENV.fetch("XDG_DATA_HOME", ENV["HOME"] + "/.local/share") + "/fonts/"
    unless Dir.exists?(fonts_dir)
        Dir.mkdir(fonts_dir)
        sh 'chmod go-w ' + fonts_dir
    end
    unless File.exists?(fonts_dir + "Quivira.otf")
        sh 'curl -O http://www.quivira-font.com/files/Quivira.otf'
        sh 'mv Quivira.otf ' + fonts_dir 
    end
    unless File.exists?(fonts_dir + "NotoEmoji-Regular.ttf")
        sh 'curl -O https://noto-website-2.storage.googleapis.com/pkgs/NotoEmoji-unhinted.zip'
        sh 'unzip NotoEmoji-unhinted.zip NotoEmoji-Regular.ttf'
        sh 'mv NotoEmoji-Regular.ttf ' + fonts_dir
        sh 'rm NotoEmoji-unhinted.zip'
    end
end

def open_pdf (demo)
    if OS.windows?
        sh "open pdf\\collections\\" + demo + "_demo.pdf"
    elseif OS.mac?
        sh "open pdf/collections/" + demo + "_demo.pdf"
    else
        sh "xdg-open pdf/collections/" + demo + "_demo.pdf 2>/dev/null | true"
    end
end

desc "Open demo pdf (all to open all of them)"
task  :open, [:demo] do |t, args|
    if "#{args[:demo]}" == "all"
        demos.each { |demo| open_pdf "#{demo}" }
    else
        open_pdf "#{args[:demo]}"
    end
end

desc "Test"
task :test => :clean
task :test => :document
task :test do
    sh 'Rscript -e "devtools::test()"'
    file = "test1"
    extra_flags = " --use_suit_as_ace --rank_symbols.chip_face='A,B,C,D,E,F' --suit_symbols.suited=^"
    internal_cfgs = " --internal_cfgs="
    make_piecepack file,  piecepack_suits + " --suit_colors=darkred,black,darkgreen,darkblue,black" + orthodox_ranks_noto + extra_flags + internal_cfgs

    file = "test2"
    extra_flags = " --pp_die_arrangement=opposites_sum_to_5 --invert_colors.suited  --background_colors=white" + star_chips
    internal_cfgs = " --internal_cfgs="
    make_piecepack file, latin_suits_swirl + default_ranks_noto + extra_flags+ internal_cfgs

    # suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    # file = "test3"
    # extra_flags = " --dm_symbols= --dm_symbols.tile_face=♥,♠,♣,♦,★ --background_colors.unsuited=orange" 
    # internal_cfgs = " --internal_cfgs="
    # make_piecepack file, suit_symbols + default_ranks_noto + extra_flags+ internal_cfgs

    # suit_symbols = " --suit_symbols=♥,♠,♣,♦,★"
    # file = "test4"
    # extra_flags = " --suit_colors=" + light_scheme + " --invert_colors.unsuited --background_colors=white --font='Deja Vu Sans'" + hexlines_light
    # internal_cfgs = " --internal_cfgs="
    # make_piecepack file, suit_symbols + default_ranks_noto + extra_flags+ internal_cfgs

    # decks = " --deck_names=test1,test2,test3,test4"
    decks = " --deck_names=test1,test2"
    sh collect_piecepacks + ' --collection_name=test_demo --title="Test' + version_str + decks
    # open_pdf "test"
end

# https://stackoverflow.com/questions/170956/how-can-i-find-which-operating-system-my-ruby-program-is-running-on
module OS
    def OS.windows?
        (/cygwin|mswin|mingw|bccwin|wince|emx/ =~ RUBY_PLATFORM) != nil
    end

    def OS.mac?
        (/darwin/ =~ RUBY_PLATFORM) != nil
    end

    def OS.unix?
        !OS.windows?
    end

    def OS.linux?
        OS.unix? and not OS.mac?
    end
end

