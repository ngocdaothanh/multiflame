class GamesController < ApplicationController
  def index
    @games = Game.find(:all)
    @games.sort! {|g1, g2| g2.updated_at <=> g1.updated_at }
  end

  def show
    @game = Game.find(params[:id])
  end

  def play
    @game = Game.find(params[:id])
  end
end