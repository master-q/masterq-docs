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
Process.setpgid(pid, Process.getpgid(0))

# setup wiimote
puts 'Put Wiimote in discoverable mode now (press 1+2)...'
wiimote = WiiMote.new
wiimote.rpt_mode = WiiMote::RPT_BTN
wiimote.led = WiiMote::LED1_ON | WiiMote::LED4_ON
puts 'OK!'

# setup druby
rabbit = DRbObject.new_with_uri("druby://localhost:10101")

leds = [WiiMote::LED1_ON, WiiMote::LED2_ON, WiiMote::LED3_ON, WiiMote::LED4_ON, WiiMote::LED3_ON, WiiMote::LED2_ON]
ledcnt = 0
gcount = 0

# main loop
buttons_prev = 0
while true
  buttons_prev = wiimote.buttons
  wiimote.get_state
  buttons_state = wiimote.buttons - (wiimote.buttons & buttons_prev)

  # led night rider
  gcount = (gcount + 1) % 20
  if gcount == 0
    ledcnt = (ledcnt + 1) % leds.size
    wiimote.led = leds[ledcnt]
  end

  case buttons_state
  when WiiMote::BTN_A
    rabbit.send(:move_to_next_if_can)
  when WiiMote::BTN_B
    rabbit.send(:move_to_previous_if_can)
  when WiiMote::BTN_RIGHT
    rabbit.send(:move_to_next_slide_if_can)
  when WiiMote::BTN_LEFT
    rabbit.send(:move_to_previous_slide_if_can)
  when WiiMote::BTN_UP
    rabbit.send(:move_to_first)
  when WiiMote::BTN_DOWN
    rabbit.send(:move_to_last)
  when WiiMote::BTN_HOME
    rabbit.send(:toggle_fullscreen)
  when WiiMote::BTN_PLUS
    rabbit.send(:toggle_index_mode)
  when WiiMote::BTN_MINUS
    rabbit.send(:toggle_whiteout)
  end

  sleep 0.1
end
