class Utils
  include Singleton

  UTIL_RANDOM = 'random'
  UTIL_SHUFFLE = 'shuffle'

  def check(arg)
    if arg.is_a?(Array)
      if arg[0] == UTIL_RANDOM
        limit = arg[1]
        return [UTIL_RANDOM, limit, random(limit)]
      elsif arg[0] == UTIL_SHUFFLE
        limit = arg[1]
        # Security check, limit cannot be too large
        raise("@shuffle: Limit is too large: #{limit}") if limit > 1024
        return [UTIL_SHUFFLE, limit, shuffle(limit)]
      end
    end
    nil
  end

private

  # in: limit
  # out: A random number from 0 to limit - 1
  def random(limit)
    rand(limit)
  end

  # in: limit
  # out: An array of limit unique numbers, from 0 to limit - 1
  def shuffle(limit)
    a = Array.new(limit)
    limit.times { |i| a[i] = i }

    ret = Array.new(limit)
    while a.size > 0
      ret[limit - a.size] = a.delete_at(rand(a.size))
    end
    ret
  end
end