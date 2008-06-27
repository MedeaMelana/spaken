# Rake build file voor Spaken


### Nuttige constanten

def plak(links, rest)
  if rest.empty? then
    ''
  else
    links + rest.join(':')
  end
end

# Cross-platform libraries
LIBS = FileList[File.join('lib', 'java', RUBY_PLATFORM, '*')]

# Native libraries
#TODO: op een nuttiger manier platform-specifiek maken:
NATIVE_LIBS = FileList[File.join('lib', 'native', RUBY_PLATFORM, '*')]

# Constueer java-invocatie
CP = (['target'] + LIBS)
JAVA = "java -ea -cp #{CP.join ':'} #{plak '-Djava.library.path=', NATIVE_LIBS}"

# Constueer javac-invocatie
JAVAC = "javac -target 1.5 #{plak '-classpath ', LIBS} -sourcepath src -d target"

# Laat deze info zien
puts "JAVA=#{JAVA}"
puts "JAVAC=#{JAVAC}"
puts


MAIN_CLASS = 'spaken.ui.swing.SpakenApp'



### Helper stuff

def system?(cmd)
  r = system cmd
  raise 'Failure' unless r
end



### Tasks

# Default task
task :default => :compile


task :init do
  mkdir 'target' unless File.exists? 'target'
  mkdir 'dist'   unless File.exists? 'dist'
end


desc 'Clean up'
task :clean

task :clean do
  rm_rf 'target'
  rm_rf 'dist'
end


desc 'Run SWT frontend'
task :run => :compile

task :run do
  system "#{JAVA} #{MAIN_CLASS}"
end


desc 'Compile SWT frontend'
task :compile => :init

task :compile do
  system? "#{JAVAC} src/#{MAIN_CLASS.gsub '.', '/'}.java"
end


desc 'Make distributable jar file'
task :dist_jar => :compile

task :dist_jar do
  rm_f 'dist/spaken.jar'
  File.open 'dist/MANIFEST.MF', 'w' do |f|
    f.puts "Main-Class: #{MAIN_CLASS}"
  end
  system? "jar cfm dist/spaken.jar dist/MANIFEST.MF -C target spaken"
  rm 'dist/MANIFEST.MF'
end
