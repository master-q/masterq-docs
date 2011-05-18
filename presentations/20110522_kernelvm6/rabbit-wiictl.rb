#!/usr/bin/ruby1.9.1
require 'rubygems'
require 'cwiid'
require 'drb/drb'

def usage
  puts "Usage: rabbit-wiictl.rb hoge.rd"
  exit 1
end

usage if ARGV.size != 1

# exec rabbit
pid = spawn"rabbit --use-druby --public-level=all #{ARGV[0]}"

# setup wiimote
puts 'Put Wiimote in discoverable mode now (press 1+2)...'
wiimote = WiiMote.new
wiimote.rpt_mode = WiiMote::RPT_BTN
puts 'OK!'

# setup druby
rabbit = DRbObject.new_with_uri("druby://localhost:10101")

# main loop
buttons_prev = 0
while true
  buttons_prev = wiimote.buttons
  wiimote.get_state
  buttons_state = wiimote.buttons - (wiimote.buttons & buttons_prev)
  rabbit.send(:move_to_previous_if_can) if buttons_state == WiiMote::BTN_LEFT
  rabbit.send(:move_to_next_if_can) if buttons_state == WiiMote::BTN_RIGHT
  rabbit.send(:move_to_first) if buttons_state == WiiMote::BTN_UP
  rabbit.send(:move_to_last) if buttons_state == WiiMote::BTN_DOWN
  rabbit.send(:toggle_fullscreen) if buttons_state == WiiMote::BTN_HOME
  rabbit.send(:toggle_index_mode) if buttons_state == WiiMote::BTN_PLUS
  sleep 0.1
end
