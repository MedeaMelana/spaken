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
LIBS = FileList[File.join('lib', 'java', '*')]

# Native libraries
#TODO: op een nuttiger manier platform-specifiek maken:
NATIVE_LIBS = FileList[File.join('lib', 'native', RUBY_PLATFORM, '*')]

# Constueer java-invocatie
CP = (['target'] + LIBS)
JAVA = "java -ea -cp #{CP.join ':'} #{plak '-Djava.library.path=', NATIVE_LIBS}"

# Constueer javac-invocatie
JAVAC = "javac -target 1.5 #{plak '-classpath ', LIBS} -sourcepath src -d target"


task :default => :compile


### Helper stuff

def system?(cmd)
  r = system cmd
  raise 'Failure' unless r
end



### Tasks

task :init do
  mkdir 'target' unless File.exists? 'target'
end


desc 'Clean up'
task :clean

task :clean do
  rm_rf 'target'
end


desc 'Run SWT frontend'
task :run => :compile

task :run do
  system? "#{JAVA} spaken.ui.swt.Main"
end


desc 'Compile SWT frontend'
task :compile => :init

task :compile do
  system? "#{JAVAC} src/spaken/ui/swt/Main.java"
end


