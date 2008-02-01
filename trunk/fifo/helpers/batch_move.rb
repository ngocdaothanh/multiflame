class BatchMove
  MAX_NUM_DEFAULT_NULLS = 5

  attr_reader :moved_indices

  def new_game(n_players0)
    @n_players = @n_players0 = n_players0
    if @moves.nil?
      @moves = Array.new
      @moved_indices = Array.new
      @resigned_indices = Array.new
      @num_default_nulls = 0
    else
      clear
    end
  end

  def clear
    @moves.clear
    @moved_indices.clear
    @resigned_indices.clear
  end

  # @return true if the batch is full and ready for broadcasting.
  def move(index, data)
    @moves << index
    @moves << data

    @moved_indices << index  # include? was called by the room
    @moved_indices.size == @n_players
  end

  # @return true if the batch is full and ready for broadcasting.
  def resign(index)
    # Removed this player's move if any
    if @moved_indices.include?(index)
      i = 0
      while i < @moves.size
        if index == @moves[i]
          @moves.delete_at(i)
          @moves.delete_at(i)
          break
        else
          i += 2
        end
      end
      @moved_indices.delete(index)
    end

    @n_players -= 1
    @resigned_indices << index
    @n_players > 0 and @moved_indices.size == @n_players
  end

  # Fill in nulls for player who did not move.
  # Return true if the number of full null fillins reaches a threshold.
  def timeout
    if @moved_indices.empty?
      @num_default_nulls += 1
    else
      @num_default_nulls = 0
    end

    @n_players0.times do |index|
      next if @resigned_indices.include?(index)
      move(index, nil) unless @moved_indices.include?(index)
    end

    @num_default_nulls == MAX_NUM_DEFAULT_NULLS
  end

  def include?(index)
    @moved_indices.include?(index)
  end

  def to_a
    @moves
  end
end