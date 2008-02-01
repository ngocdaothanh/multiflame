class Utils
  include Singleton

  def check(arg)
    if arg.is_a?(Array)
      return my_rand(arg) if arg[0] == 'rand'
      return shuffle(arg) if arg[0] == 'shuffle'
    end

    nil
  end

  # in: limit
  # out: A random number from 0 to limit - 1
  def my_rand(arg)
    limit = arg[1]
    ['rand', limit, rand(limit)]
  end

  # in: limit
  # out: An array of limit unique numbers, from 0 to limit - 1
  def shuffle(arg)
    limit = arg[1]

    # Security check, limit cannot be too large
    raise("@shuffle: Limit is too large: #{limit}") if limit > 1024

    a = Array.new(limit)
    limit.times { |i| a[i] = i }

    a_rand = Array.new(limit)
    while a.size > 0
      a_rand[limit - a.size] = a.delete_at(rand(a.size))
    end
    ['shuffle', limit, a_rand]
  end
end