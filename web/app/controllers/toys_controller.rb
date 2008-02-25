require 'revent/captcha'
require 'revent/amf3/amf3'

class JPGMailer < ActionMailer::Base
  def msg(recipient, subject, body, jpg)
    self.from       CONFIG[:admin_email]
    self.recipients recipient
    self.subject    subject
    self.body       body
    unless jpg.nil?
      self.attachment :content_type => 'image/jpeg', :body => jpg
    end
  end
end

class ToysController < ApplicationController
  def index
    @toys = Toy.find(:all)
  end

  # Show the toy and let user config.
  def show
    @toy = Toy.find(params[:id])
  end

  # Utilities require captcha.
  def captcha
    encrypted_code_with_timestamp, img = captcha_obj.new
    ba1 = RubyAMF::IO::ByteArray.new(encrypted_code_with_timestamp)
    ba2 = RubyAMF::IO::ByteArray.new(img)
    enc = RubyAMF::IO::AMFSerializer.new
    enc.write_amf3([ba1, ba2])
    send_data enc.stream, :type => 'application/x-amf3-object', :disposition => 'inline'
  end

  def email
    dec = RubyAMF::IO::AMFDeserializer.new
    dec.stream = request.raw_post
    o = dec.read_amf3

    encrypted_code_with_timestamp = o[:encryptedCode]
    code                          = o[:code]
    email                         = o[:email]
    subject                       = o[:subject]
    body                          = o[:body]
    jpg                           = o[:jpg]
p 'code', code
p 'encrypted_code_with_timestamp', encrypted_code_with_timestamp
    #if captcha_obj.correct?(code, encrypted_code_with_timestamp)
      p email
      JPGMailer.deliver_msg(email, subject, body, jpg)
    #end
    render :nothing => true
  end

private

  def captcha_obj
    @captcha_obj ||= Revent::Captcha.new('ss', 6, 24*60*60)
  end
end
