require 'md5'
require 'RMagick'

class Captcha
  include Singleton
  include Magick

  CODE_LENGTH = 3
  JIGGLE = 15
  WOBBLE = 20

  def initialize
    @salt = 'xxx'
  end

  def new
    code, img = private_new
    encrypted_code = Digest::MD5.hexdigest(@salt + code)
    [encrypted_code, img]
  end

  def correct?(code, encrypted_code)
    encrypted_code0 = Digest::MD5.hexdigest(@salt + code)
    encrypted_code0 == encrypted_code
  end

private

  def private_new
    chars = ('A'..'Z').to_a
    code_array = []
    1.upto(CODE_LENGTH) { code_array << chars[rand(chars.length)] }
    granite = Magick::ImageList.new('granite:')
    canvas = Magick::ImageList.new
    canvas.new_image(33*CODE_LENGTH, 40, Magick::TextureFill.new(granite))
    text = Magick::Draw.new
    #text.font_family = 'times'
    text.pointsize = 40
    cur = 10

    code_array.each do |c|
      rot = rand(10) > 5 ? rand(WOBBLE): -rand(WOBBLE)
      text.annotate(canvas, 0, 0, cur, 25 + rand(JIGGLE), c) do
        self.rotation = rot
        self.fill = 'darkred'
      end
      cur += 30
    end
    code = code_array.to_s
    img = canvas.to_blob { self.format = 'JPG'; self.quality = 60 }
    [code, img]
  end
end