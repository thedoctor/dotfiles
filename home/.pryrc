Pry.editor = 'emacs'


Pry.commands.alias_command 'c', 'continue' rescue nil
Pry.commands.alias_command 's', 'step' rescue nil
Pry.commands.alias_command 'n', 'next' rescue nil
Pry.commands.alias_command 'r!', 'reload!' rescue nil

Pry.prompt = [proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} > " }, proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} * " }]

# === Listing config ===
# Better colors - by default the headings for methods are too
# similar to method name colors leading to a "soup"
# These colors are optimized for use with Solarized scheme
# for your terminal

Pry.config.ls.separator = "\n" # new lines between methods
Pry.config.ls.heading_color = :magenta
Pry.config.ls.public_method_color = :green
Pry.config.ls.protected_method_color = :yellow
Pry.config.ls.private_method_color = :bright_black

# == PLUGINS ===
# awesome_print gem: great syntax colorized printing
# look at ~/.aprc for more settings for awesome_print
#require 'interactive_editor'
# begin
#   require 'rubygems'
#   require 'awesome_print'
#   require 'awesome_print_colors'

# # AwesomePrint.defaults={
# #               :theme=>:solorized,
# #               :indent => 2,
# #               :sort_keys => true,
# #               :color => {
# #                 :args       => :greenish,
# #                 :array      => :pale,
# #                 :bigdecimal => :blue,
# #                 :class      => :yellow,
# #                 :date       => :greenish,
# #                 :falseclass => :red,
# #                 :fixnum     => :blue,
# #                 :float      => :blue,
# #                 :hash       => :pale,
# #                 :keyword    => :cyan,
# #                 :method     => :purpleish,
# #                 :nilclass   => :red,
# #                 :string     => :yellowish,
# #                 :struct     => :pale,
# #                 :symbol     => :cyanish,
# #                 :time       => :greenish,
# #                 :trueclass  => :green,
# #                 :variable   => :cyanish
# #             }
# #          }



#   AwesomePrint.defaults={:theme=>:solorized}


#   Pry.config.print = proc {|output, value| Pry::Helpers::BaseHelpers.stagger_output("=> #{value.ai}", output)}

# # If you want awesome_print without automatic pagination, use the line below
# # Pry.config.print = proc { |output, value| output.puts value.ai }
# rescue LoadError => err
#   puts err
#   puts "gem install awesome_print  # <-- highly recommended"
# end

begin
  require 'pry-clipboard'
  Pry.config.commands.alias_command 'ch', 'copy-history'
  Pry.config.commands.alias_command 'cr', 'copy-result'
rescue LoadError => e
  warn "can't load pry-clipboard"
end

default_command_set = Pry::CommandSet.new do
  command "copy", "Copy argument to the clip-board" do |str|
     IO.popen('pbcopy', 'w') { |f| f << str.to_s }
  end
end

Pry.config.commands.import default_command_set

def cli_to_ipy(fn)
    d={}
    text = File.open(fn, 'r')
    text.each_line do |l|
      case l[0,2]
      when '-a'
        d['apitoken']=l[2..-2]
      when '-m'
        d['admintoken']=l[2..-2]
      when '-d'
        d['devicetoken']=l[2..-2]
      when '-u'
        d['url']=l[2..-2]
      when '-e'
        d['email']=l[2..-2]
      end
    end
    puts d
    d
end

c = user = app = nil

def gogo(context='usr')
  d = cli_to_ipy('/Users/matt/.gemwallet')
  require 'round'
  c = Round.client(d['url']) if d['url']
  c ||= Round.client
  raise StandardError if context != 'usr'
  usr = c.authenticate_device(email: d['email'],
                              api_token: d['apitoken'],
                              device_token: d['devicetoken'])
  puts "usr: #{usr}"
  usr
rescue
  app = c.authenticate_application(api_token: d['apitoken'],
                                   admin_token: d['admintoken'])
  puts "app: #{app}"
  app
end
