# encoding: utf-8

desc "Update package documentation"
task :document do
    sh 'Rscript -e "suppressMessages(devtools::document())"'
    Rake::Task["install"].invoke
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

task :test do
    sh 'Rscript -e "devtools::test()"'
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

