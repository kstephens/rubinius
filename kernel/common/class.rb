##
# Classes in Ruby are first-class objects, each is an instance of
# class Class.
#
# When a new class is created (typically using <tt>class Name; ... end</tt>),
# an object of type Class is created and assigned to a global constant (Name
# in this case). When <tt>Name.new</tt> is called to create a new object, the
# new method in Class is run by default.
#
# This can be demonstrated by overriding new in Class:
#
#   class Class
#     alias old_new new
#     def new(*args)
#       puts "Creating a new #{self.name}"
#       old_new(*args)
#     end
#   end
#
#   class Name
#   end
#
#   n = Name.new
#
# *produces:*
#
#   Creating a new Name
#
# Classes, modules, and objects are interrelated. In the diagram that follows,
# the vertical arrows represent inheritance, and the parentheses meta-classes.
# All metaclasses are instances of the class Class.
#
#                            +------------------+
#                            |                  |
#              Object---->(Object)              |
#               ^  ^        ^  ^                |
#               |  |        |  |                |
#               |  |  +-----+  +---------+      |
#               |  |  |                  |      |
#               |  +-----------+         |      |
#               |     |        |         |      |
#        +------+     |     Module--->(Module)  |
#        |            |        ^         ^      |
#   OtherClass-->(OtherClass)  |         |      |
#                              |         |      |
#                            Class---->(Class)  |
#                              ^                |
#                              |                |
#                              +----------------+

class Class

  protected :instance_type

  def initialize(sclass=Object, name=nil, under=nil)
    if !sclass.kind_of?(Class) and !sclass.nil?
      raise TypeError, "superclass must be a Class (#{sclass.class} given)"
    end

    set_superclass sclass

    super()

    # Things (rails) depend on the fact that a normal class is in the constant
    # table and have a name BEFORE inherited is run.
    set_name_if_necessary name, under if name and under
    under.const_set name, self if under

    if sclass
      # add class to sclass's subclass list, for ObjectSpace.each_object(Class)
      # NOTE: This is non-standard; Ruby does not normally track subclasses
      sclass.__send__ :add_subclass, self

      sclass.__send__ :inherited, self
    end
  end

  ##
  # Returns the Class object that this Class inherits from. Included Modules
  # are not considered for this purpose.

  def superclass()
    cls = direct_superclass
    return nil unless cls
    while cls and cls.kind_of? Rubinius::IncludedModule
      cls = cls.direct_superclass
    end
    return cls
  end

  def add_subclass(cls)
    @subclass_objects ||= []
    @subclass_objects << cls
  end
  private :add_subclass

  def __subclasses__
    @subclass_objects || []
  end

  def inherited(name)
  end
  private :inherited

  def to_s
    if obj = __metaclass_object__
      "#<Class: #{obj.inspect}>"
    else
      @module_name ? @module_name.to_s : super
    end
  end

  alias_method :inspect, :to_s
end
