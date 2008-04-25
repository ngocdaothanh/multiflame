class JPGMailer < ActionMailer::Base
  def msg(recipient, subject, body, jpg)
    self.from       CONF[:admin_email]
    self.recipients recipient
    self.subject    subject
    self.body       body
    unless jpg.nil?
      self.attachment(:content_type => 'image/jpeg', :body => jpg)
    end
  end
end
