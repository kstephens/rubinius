module DefinedSpecs
  self::SelfScoped = 42

  def self.side_effects
    ScratchPad.record :defined_specs_side_effects
  end

  def self.fixnum_method
    ScratchPad.record :defined_specs_fixnum_method
    42
  end

  def self.exception_method
    ScratchPad.record :defined_specs_exception
    raise "defined? specs exception method"
  end

  def self.defined_method
    DefinedSpecs
  end

  class Basic
    A = 42

    def defined_method
      DefinedSpecs
    end

    def a_defined_method
    end

    def local_variable_defined
      x = 2
      defined? x
    end

    def instance_variable_undefined
      defined? @instance_variable_undefined
    end

    def instance_variable_read
      value = @instance_variable_read
      defined? @instance_variable_read
    end

    def instance_variable_defined
      @instance_variable_defined = 1
      defined? @instance_variable_defined
    end

    def global_variable_undefined
      defined? $defined_specs_global_variable_undefined
    end

    def global_variable_read
      value = $defined_specs_global_variable_read
      defined? $defined_specs_global_variable_read
    end

    def global_variable_defined
      $defined_specs_global_variable_defined = 1
      defined? $defined_specs_global_variable_defined
    end

    def class_variable_undefined
      defined? @@class_variable_undefined
    end

    def class_variable_defined
      @@class_variable_defined = 1
      defined? @@class_variable_defined
    end

    def yield_defined_method
      defined? yield
    end

    def yield_defined_parameter_method(&block)
      defined? yield
    end

    def no_yield_block
      yield_defined_method
    end

    def no_yield_block_parameter
      yield_defined_parameter_method
    end

    def yield_block
      yield_defined_method { 42 }
    end

    def yield_block_parameter
      yield_defined_parameter_method { 42 }
    end
  end

  module Mixin
    MixinConstant = 42
  end

  class Parent
    ParentConstant = 42
  end

  class Child < Parent
    include Mixin

    A = 42

    def self.parent_constant_defined
      defined? self::ParentConstant
    end

    def self.module_defined
      defined? Mixin
    end

    def self.module_constant_defined
      defined? MixinConstant
    end
  end

  class Superclass
    def yield_method
      yield
    end

    def method_no_args
    end

    def method_args
    end

    def method_block_no_args
    end

    def method_block_args
    end

    def define_method_no_args
    end

    def define_method_args
    end

    def define_method_block_no_args
    end

    def define_method_block_args
    end
  end

  class Super < Superclass
    def no_super_method_no_args
      defined? super
    end

    def no_super_method_args
      defined? super()
    end

    def method_no_args
      defined? super
    end

    def method_args
      defined? super()
    end

    def no_super_method_block_no_args
      yield_method { defined? super }
    end

    def no_super_method_block_args
      yield_method { defined? super() }
    end

    def method_block_no_args
      yield_method { defined? super }
    end

    def method_block_args
      yield_method { defined? super() }
    end

    define_method(:no_super_define_method_no_args) { defined? super }
    define_method(:no_super_define_method_args) { defined? super() }
    define_method(:define_method_no_args) { defined? super }
    define_method(:define_method_args) { defined? super() }

    define_method(:no_super_define_method_block_no_args) do
      yield_method { defined? super }
    end

    define_method(:no_super_define_method_block_args) do
      yield_method { defined? super() }
    end

    define_method(:define_method_block_no_args) do
      yield_method { defined? super }
    end

    define_method(:define_method_block_args) do
      yield_method { defined? super() }
    end
  end

  class ClassWithMethod
    def test
    end
  end

  class ClassUndefiningMethod < ClassWithMethod
    undef :test
  end

  class ClassWithoutMethod < ClassUndefiningMethod
    # If an undefined method overridden in descendants
    # define?(super) should return nil
    def test
      defined?(super)
    end
  end
end

class Object
  def defined_specs_method
    DefinedSpecs
  end

  def defined_specs_receiver
    DefinedSpecs::Basic.new
  end
end
