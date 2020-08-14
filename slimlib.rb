# coding: utf-8
require "slim/include"

Slim::Engine.set_options(:disable_escape => true,
                        :disable_capture => false)

def include_slim(name, options = {}, &block)
  t = Slim::Template.new("#{name}.slim", options)
  if block_given?
    t.render(self, options, &block)
  else
    t.render(self, options)
  end
end

def capture_to(var, &block)
  t = Slim::Template.new { "  ==yield" }
  c = t.render(self, {}, &block)
  block.binding.local_variable_set(var, c)
  # set_var.call(block)
  # # In Rails we have to use capture!
  # # If we are using Slim without a framework (Plain Tilt),
  # # you can just yield to get the captured block.

  # # a = block.call
  # return ''
end

def capture_block &block
  r = block.call
  # return r
  return "aaa"
end

def capture_block_to(var, &block)
  block.binding.local_variable_set(var, block.call)
  return nil
  # In Rails we have to use capture!
  # If we are using Slim without a framework (Plain Tilt),
  # you can just yield to get the captured block.
  # set_var.call(yield)
end
#
#
# hakyll template wrapper
#
#
def field name
  return "$#{name.to_s}$"
end

def _unless cond, &block
  return "$if(#{cond})$$else$\n#{yield}\n"
end


def _if cond, &block
  return "$if(#{cond})$\n#{yield}\n"
end

def _else &block
  return "$else$\n#{yield}\n"
end

def _endif
  return "$endif$\n"
end

def _for stmt, &block
  return "$for(#{stmt})$\n#{yield}\n"
end

def _endfor
  return "$endfor$\n"
end
