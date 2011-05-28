require 'rubygems'
require 'twitter/json_stream'
require 'json'

theme_exit unless display?

proc_name = "twitter-footer"

if @twitter_footer_auto_update.nil?
  @twitter_footer_auto_update = true
end

@twitter_footer_props ||= {
  "size" => screen_size(1.4 * Pango::SCALE),
  "font_family" => @font_family,
}
@twitter_footer_color ||= "#ffffff"
@twitter_userpass ||= File.open("#{ENV['HOME']}/.twitter_userpass").read.gsub(/\s/,"")
@twitter_stream_content ||='track=#hoge'

@twitter_stream_tweets = []

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  stop_auto_redraw_timer

  break if @twitter_footer_uninstall

  if @twitter_footer_auto_update
    start_auto_redraw_timer(1)
  end
  
  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      unless @twitter_stream_tweets.empty?
        text = Text.new(@twitter_stream_tweets.first.chomp("").gsub("\n","\\"))
        @twitter_stream_tweets.shift if @twitter_stream_tweets.size > 1
        text.font @twitter_footer_props
        set_font_family(text)
        text.compile(canvas, x, y, w, h)
        text.layout.set_width(w * Pango::SCALE)
        num_x = x
        num_y = canvas.height - 20
        canvas.draw_layout(text.layout, num_x, num_y, @twitter_footer_color)
      end
    end
    [x, y, w, h]
  end
end

# pull twitter stream
Thread.start do
  EventMachine::run {
    stream = Twitter::JSONStream.connect(
      :path    => '/1/statuses/filter.json',
      :auth    => @twitter_userpass,
      :method  => 'POST',
      :content => @twitter_stream_content
    )
    
    stream.each_item do |item|
      status = JSON.parse(item) rescue next
      tweet = "@#{status['user']['screen_name']}:\"#{status['text']}\"\n"
#      $stdout.print tweet
#      $stdout.flush
      @twitter_stream_tweets.push tweet
    end
  
    stream.on_error do |message|
      $stdout.print "error: #{message}\n"
      $stdout.flush
    end
  
    stream.on_reconnect do |timeout, retries|
      $stdout.print "reconnecting in: #{timeout} seconds\n"
      $stdout.flush
    end

    stream.on_max_reconnects do |timeout, retries|
      $stdout.print "Failed after #{retries} failed reconnects\n"
      $stdout.flush
    end

#    trap('TERM') {  
#      stream.stop
#      EventMachine.stop if EventMachine.reactor_running? 
#    }
  }
end
