class ToysController < ApplicationController
  def index
    @toys = Toy.find(:all)
  end

  # Show the toy and let user config.
  def show
    @toy = Toy.find(params[:id])
  end
end
