class Point
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def dist_from_origin
    Math.sqrt(@x * @x + @y * @y)
  end

  def dist_from_origin_2
    Math.sqrt(x * x + y * y) # uses getter methods
  end
end

class ColorPoint < Point
  attr_accessor :color

  def initialize(x, y, c="clera")
    super(x, y)
    @color = c
  end
end