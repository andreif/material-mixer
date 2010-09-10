=begin TODO:

+. inherit density if one compound and the same temperature
+. prefer natural composition to mix unless composition is redefined
+. remove missing nuclides and recalc fractions
?. material aliases - do not repeat compositions if tmp and dens is the same
? solve if natural available for other lib, and there are no partial nuclides for requested lib
2. use function of density for some materials (water, lead, lbe) - again if one compound then inherit the function
3. use density multiplier to simulate void
4. detect thermal library - also create library for thermal materials
5. volume fractions, different volume models
6. mixing densities, crystal structures and different phases

Cards for Serpent?

set mixes "
Pu  Pu-239  .5
    Pu-240  .5
"

mat Pu 0
Pu-239  .5
Pu-240  .5

mat fuel -2
Pu 0.5

=end

class MaterialMixer
  attr_accessor :mixes
  
  
  def initialize text_with_mixes=nil
    @mixes = {}
    self.parse NATURAL_COMPOSITIONS, :natural # old way: self.load_natural_compositions
    self.parse LIBRARY_COMPOSITIONS, :library
    self.parse text_with_mixes, :user_defined unless text_with_mixes.nil?
    yield self if block_given?
  end
  
  
  
  
  def get arg
    case arg
      when Hash
        type, name = arg.keys.first, arg.values.first
        name = name.to_s
        case type
          when :mix     then r = @mixes[name] ||= MaterialMix.new(name, self)
          when :nuclide then r = Nuclide.get(name)
        else raise end
      when String,Symbol
        name = arg.to_s
        if @mixes.has_key? name or not Nuclide.valid? name
          r = self.get(mix: name)
        else
          r = self.get(nuclide: name)
        end
    else raise end
    yield r if block_given?
    return r
  end
  def mix name; self.get(mix: name) end
  
  def fmt fm, var
    var = var.to_f
    r = fm % var.to_f
    case fm
      when /\%(\d*).(\d+)f/
        return self.fmt("%#{$1}e",var) if ($2.to_i > 10 and var < 10**(10-$2.to_i)) or (var < 10**(-$2.to_i))
        case r
          when /\.(\d*9{4,})$/ then r = fm % r.to_f.round($1.length-2)
          when /\.\d*(0{3,}1)$/ then r = r[0,r.length - $1.length] + ' '*$1.length
        end
        r.gsub! /#{$1}$/, ' '*$1.length if r =~ /(0+\s*)$/#/[^.](0+)$/
    end
    return r
  end
  
  
  def correct_missing_nuclides! mix_nuclides, mix_temperature
    missing, corrections, found, modified = {}, {}, {}, {}
    mix_nuclides.each_pair do |nuclide,fractions|
      element = nuclide.element_symbol
      if xs = self.xs_key(nuclide.zaid, mix_temperature)
        found[element] ||= {}
        found[element][nuclide] = fractions
      else
        missing[element] ||= {}
        missing[element][nuclide] = fractions.dup # must copy
        [:atomic,:mass].each do |type|
          corrections[element] ||= {}
          corrections[element][type] ||= 0
          corrections[element][type] += fractions[type]
          fractions[type] = 0 # clear for missing
        end
        modified[element] ||= {updated: [], removed: []}
        modified[element][:removed].push nuclide.name
      end
    end
    missing.each_pair do |element,nuclides|
      raise "Element #{element} is missing completely" unless found[ element ]
    end
    found.each_pair do |element,nuclides|
      if missing[element]
        nuclides.each_pair do |nuclide,fractions|
          [:atomic,:mass].each do |type|
            fractions[type] *= 1 + corrections[element][type]
          end
          modified[element][:updated].push nuclide.name
        end
        #modified[element] = {updated: found[element].keys, removed: missing[element].keys}
      end
    end
    return modified, mix_nuclides
  end
  
  
  
  def stat_xs_params mix_nuclides, mix_temperature
    stats = {}
    unless @xsdata.nil? or @xsdata.data.empty?
      mix_nuclides.keys.each do |nuclide|
#p nuclide.zaid
        if xs = self.xs_key(nuclide.zaid, mix_temperature)
          [:temperature_MeV,:key,:library].each do |p|
            stats[p] ||= Hash.new(0)
            stats[p][ xs[p] ] += 1
          end
        end
      end
      by_value = Proc.new {|a,b| a.last <=> b.last}
      [:temperature_MeV,:key,:library].each do |p|
        value,count = stats[p].sort(&by_value).last
        stats[p] = (count > 1 or mix_nuclides.count < 2) ? value : nil
      end
    end
    return stats
    # procedure to count array items: array_stat = Proc.new {|h,item| h[item]+=1; h}
  end
  
  def printable_mixnames
    @mixes.select { |name,mix|  mix.density && (mix.density > 0) && (not mix.alias?) && mix.user_defined? }.keys
    #@mixes.each_pair { |name,mix| r += self.print_for_mcnp(name) unless mix.density.nil? or mix.density.zero? }
  end
  def print_for_mcnp *mixnames; self.print_for :mcnp, *mixnames end
  def print_for_serpent *mixnames; self.print_for :serpent, *mixnames end
  def print_for code, *mixnames
    mixnames = self.printable_mixnames if mixnames.empty?
    return mixnames.collect { |mixname| self.print self.get(mix: mixname), code if @mixes.has_key? mixname.to_s }.join
  end
  
  
  def get_element_sums nuclides
    sums = {}
    nuclides.each_pair do |nuclide,fractions|
      sums[ nuclide.symbol ] ||= {}
      [:atomic,:mass].each do |type|
        sums[ nuclide.symbol ][type] ||= 0
        sums[ nuclide.symbol ][type] += fractions[type]
      end
    end
    return sums
  end
  
  
  
  
  def print mix, code= :mcnp
    r = ''
#p mix.name
    stats = self.stat_xs_params mix.nuclides, mix.temperature
#p stats
    @mat_count ||= 0
    case code
      when :mcnp
        c = 'c'; ic = '$'
        r += "M%-3d %s #{ic} name= %s, temperature= %s%s%s\n" % [
          @mat_count += 1,
          stats[:key] ? 'nlib=%s' % stats[:key] : nil,
          mix.name,
          mix.temperature,
          stats[:temperature_MeV] ? ', %s MeV' % stats[:temperature_MeV] : nil,
          stats[:library] ? ', library file= ' % stats[:library] : nil
        ]
      when :serpent
        c = ic = '#' # comment symbols
        r += "\nmat %s %s #{ic} temperature= %s \n" % [
          mix.name,
          -mix.density.to_f,
          mix.temperature
        ]
    else raise "Unknown format for printing: #{code}" end
    r += "#{c} avg.mass= %.2f u" % mix.average_atomic_mass
    unless mix.density.nil? or mix.density.zero?
      r += ", density= %s g/cm3, at.density= %.4e /b/cm" % [
        self.fmt('%12.10f', mix.density).strip,
        mix.atomic_density
      ]
    end
    r += "\n"
    elements = mix.elements
    mix.stat_compounds.merge(mix.elements).each_pair do |element,fractions|
      r += "#{c} %-3s %s at%%, %s wt%%, %6.2f u" % [
        element,
        self.fmt('%7.3f', 100*fractions[:atomic]),
        self.fmt('%7.3f', 100*fractions[:mass]),
        self.get(element).average_atomic_mass(fractions[:nuclides])
      ]
      r += ", %.4e /b/cm" % (mix.atomic_density * fractions[:atomic]) unless mix.density.nil? or mix.density.zero?
      r += "\n"
    end
    nuclides = mix.nuclides # do not move it
    modified, nuclides = self.correct_missing_nuclides! nuclides, mix.temperature # careful, it modifies nuclides!
    nuclides.each_pair do |nuclide,fractions|
      if xs = self.xs_key(nuclide.zaid, mix.temperature)
        r += "%s%6s%-4s   %s #{ic} %-7s %8.5f wt%%, %6.2f u, %s at%% in %-2s" % [
          {mcnp:' '*5}[code],
          nuclide.zaid,
          (stats[:key].nil? or xs[:key] == stats[:key]) && (code== :mcnp) ? nil : '.'+xs[:key].to_s,
          self.fmt('%17.14f', 100*fractions[:atomic]),
          nuclide.name,
          100*fractions[:mass],
          nuclide.average_atomic_mass,
          self.fmt('%7.3f', 100*elements[nuclide.symbol][:nuclides][nuclide][:atomic]/elements[nuclide.symbol][:atomic]),
          nuclide.symbol
        ]
        r += ", %.4e /b/cm" % (mix.atomic_density * fractions[:atomic]) unless mix.density.nil? or mix.density.zero?
        r += ", %s MeV" % xs[:temperature_MeV] unless stats[:temperature_MeV].nil? or xs[:temperature_MeV] == stats[:temperature_MeV]
        r += ", %s" % xs[:library] unless stats[:library].nil? or xs[:library] == stats[:library]
        r += "\n"
      else
        r += "%35s #{ic} warning: %s is missing in the provided XS-directory file. Fraction of %s in %s was corrected\n" % [nil,
          nuclide.name,
          (modified[ nuclide.element_symbol ][:updated].join(', ') rescue 'no nuclides'),
          nuclide.element_symbol
        ]
      end
    end
    return r
  end
  
  
  
  def parse text, group= :user_defined
    text = text.gsub(/#[^\n]*\n/, "\n").gsub("\t",' '*4) + "\n  \n"
    # collect card words
    cards = []
    text.strip.split("\n").each do |line|
      next if line.strip.empty?
      match,indent,line = *(/^([ ]*)(.*)$/.match line)
      words = line.split
      if indent.length > 5
        cards.last.push(words).flatten! unless cards.empty?
      else
        cards.push words
      end
    end
    # process card words
    cards.each do |words|
      # extract parameters
      parameters = {}
      words.select { |word| word =~ /=/ }.each do |keyvalue|
        key,value = keyvalue.split('=')
        key = {'t'=>:temperature,'d'=>:density}[key.downcase] || key.downcase.to_sym
        parameters[key] = value
      end
      words = words.delete_if { |word| word =~ /=/ }
      
      # collect compounds
      if words.length < 3
        raise "Not complete mix"
      else
        # add mix and compounds
        self.get(mix: words.shift) do |mix|
          mix.reset group # clear settings
          af_sum,mf_sum = 0,0
          balanced = []
          
          words.each_slice(2) do |pair|
            name,fr = pair
            mix.add_compound self.get(name) do |c|
              if fr =~ /balance/i
                # save name of compound
                balanced.push name
              elsif (fr = fr.to_f) >= 0
                c.fraction atomic: fr
                af_sum += fr
              else
                c.fraction mass: fr.abs
                mf_sum += fr.abs
              end
            end
          end
          # if balance is requested
          if not balanced.empty?
            if balanced.count > 1 then raise "balance used more than once"
            elsif af_sum > 0 and mf_sum > 0 then raise "Both atomic and mass fraction"
            elsif af_sum > 0 then type = :atomic; fr = af_sum
            elsif mf_sum > 0 then type = :mass; fr = mf_sum
            else raise "balance for one compound" end
            balanced = balanced.first
            if fr >= 100 then raise "balance sum >= 100"
            elsif fr > 1 then 
              mix.compounds[balanced].fraction(type => 100-fr)
            elsif fr.equal? 1.0 then raise "sum is 1.0"
            else
              mix.compounds[self.get(balanced).name].fraction(type => (1-fr)) end
          end
          mix.temperature = parameters[:temperature]
          mix.density = parameters[:density].to_f.abs unless parameters[:density].nil?
#p [mix.name, mix.temperature]
        end
      end
    end
    @mixes.values.each {|mix| mix.inherit_properties}
  end
  
  
  # def load_natural_compositions
  #   NATURAL_COMPOSITIONS.strip.split("\n").each do |line|
  #     element_symbol, mass_number, af = line.split # skip half-lifes
  #     
  #     nuclide = self.get nuclide: (element_symbol + mass_number)
  #     
  #     self.get(mix: element_symbol) do |mix|
  #       mix.add_compound(nuclide).fraction atomic: af.to_f
  #       mix.mark_as_natural
  #     end
  #     
  #   end
  #   @mixes.values.each do |mix| 
  #     mix.normalize_fractions
  #   end
  # end
  
  def load_library args
    text = IO.read args[:path] unless args[:path].nil?
    @xslib = args[:filter]
    @xsdata = (text =~ /^directory/ ? XSDIR : XSDATA).new.parse text
    return self
    # also thermal libraries ??
  end
  
  def xs_key zaid, temperature, exact_only=false
    zaid = self.get(nuclide:zaid).zaid
#p [zaid, @xsdata.nil?, @xsdata.data.empty?, temperature.nil?]
    if @xsdata.nil? or @xsdata.data.empty? or temperature.nil?
      return exact_only ? nil : {}
    end
    return nil if @xsdata.data[zaid].nil?
#p zaid
    lib_alternative = tmp_alternative = any_alternative = nil
    @xsdata.data[zaid].each_pair do |key,xs|
      t0 = temperature.to_i
      t1 = xs[:temperature_Kelvin].to_i
      t2 = tmp_alternative[:temperature_Kelvin].to_i unless tmp_alternative.nil?
      
      temperature_match = (t0 - t1).abs < 20
      library_match = @xslib.nil? || (xs[:library] =~ @xslib)

#p match: [temperature_match, library_match, xs[:library], @xslib] if zaid == '82000'
      
      if temperature_match and library_match # exact match
        return xs
      elsif temperature_match
        lib_alternative ||= xs
      elsif library_match
        tmp_alternative = xs if tmp_alternative.nil? or (t0 - t2).abs > (t0 - t1).abs # sort by distance
      else
        any_alternative ||= xs
      end
    end
    return nil if exact_only
    return (lib_alternative or tmp_alternative or any_alternative)
  end

  
  
  class Library
    attr_reader :data
    def initialize path=nil, filter=nil
      @data = {}
      self.parse IO.read path unless path.nil?
    end
    #def add_meta zaid; {95342=>'95242m',95344=>'95244m'}[zaid.to_i] || zaid end
  end
  
  
  class XSDIR < Library
    def parse text
      lines = text.split(/directory/i).last.strip.split("\n")
      until lines.empty?
        line = ''
        while (line += ' '+lines.shift).strip! =~ /\+$/
          line.gsub! /\+$/, ''
        end
        zaid,key = (words = line.split).first.split('.')
        next unless key =~ /c/ # skip other libraries
        lib,mev = words.values_at(2,9)
        #zaid = self.add_meta zaid
        @data[zaid] ||= {}
        @data[zaid][key] = {
          zaid: zaid,
          key: key,
          library: lib,
          temperature_MeV: mev,
          temperature_Kelvin: mev.to_f * 1.1604505e10 # convert MeV -> K
        }
      end
      return self
    end
  end
  
  
  class XSDATA < Library
    def parse text
      text.strip.split("\n").each do |line|
        id,kelvin,lib = line.split.values_at(1,6,8)
        zaid,key = id.split('.')
        #zaid = self.add_meta zaid
        @data[zaid] ||= {}
        @data[zaid][key] = {
          zaid: zaid,
          key: key,
          library: lib,
          temperature_MeV: kelvin.to_f / 1.1604505e10, # convert Kelvin to MeV
          temperature_Kelvin: kelvin
        }
      end
      return self
    end
  end
  
  
  
  
  
  
  class CompoundFraction
    attr_accessor :compound, :mass
    def initialize compound
      @compound = compound
      @atomic_fraction = nil
      @mass_fraction = nil
      @mass = nil
    end
    
    def is_mix?; (@compound.class.to_s =~ /nuclide/i).nil? end
    
    def average_atomic_mass; @compound.average_atomic_mass end
    def name; @compound.name end
    
    def fraction arg
      case arg
        when Hash
          @atomic_fraction = arg[:atomic].to_f   if arg.has_key? :atomic
          @mass_fraction   = arg[:mass].to_f.abs if arg.has_key? :mass
          self.calculate_missing_fractions
        when Symbol
          case arg
            when :atomic
              unless @atomic_fraction.nil?
                return @atomic_fraction
              else raise end
            when :mass
              unless @mass_fraction.nil?
                return @mass_fraction
              else raise end
          else raise end
      else raise end
    end
    
    def calculate_missing_fractions
      if @atomic_fraction.nil? and not @mass_fraction.nil?
        @atomic_fraction = @mass_fraction.to_f / @compound.average_atomic_mass
        
      elsif @mass_fraction.nil? and not @atomic_fraction.nil?
        @mass_fraction = @atomic_fraction.to_f * @compound.average_atomic_mass
      end
    end
  end
  
  
  
  
  
  
  
  
  
  
  class MaterialMix
    attr_accessor :density, :name, :temperature
    attr_reader :compounds

    def initialize name, mixer
      @name = name
      @mixer = mixer
      self.reset :undefined
    end
    
    def mev
      @mev ||= @mixer.stat_xs_params(self.nuclides, @temperature)[:temperature_MeV]
    end
    
    def reset group
      @compounds = {}
      @group = group
      @average_atomic_mass = nil
    end
    
    def inherit_properties
      return unless @density.nil? or @temperature.nil?
      if @compounds.count.equal?(1) and @compounds.values.first.is_mix?
        (parent = @compounds.values.first.compound).inherit_properties
        @density ||= parent.density
        @temperature ||= parent.temperature
      end
      @temperature ||= '300K'
    end
    
    #def mark_as_natural; @natural = true end
    def natural?; @group == :natural end
    def user_defined?; @group == :user_defined end
    def alias?; @compounds.count.equal?(1) && @compounds.values.first.is_mix? end

    def add_compound compound
      @compounds[ compound.name ] = cf = CompoundFraction.new(compound)
      yield cf if block_given?
      return cf
    end

    
    def normalize_fractions
      raise "Mix '#{@name}' has no compounds" if @compounds.empty?
      af_sum, mf_sum = 0, 0
      @compounds.values.each do |c|
        af_sum += c.fraction(:atomic)
        mf_sum += c.fraction(:mass)
      end
      @compounds.values.each do |c|
        c.fraction atomic: (c.fraction(:atomic) / af_sum), mass: (c.fraction(:mass) / mf_sum)
      end
    end


    def average_atomic_mass nuclides=nil
      af_sum = 0
      unless nuclides.nil? # mass by list not mix compounds - do not save it!!!
        mass = 0
#p nuclides
        nuclides.each_pair do |name,fractions|
          mass += fractions[:atomic] * @mixer.get(nuclide: name).average_atomic_mass
          af_sum += fractions[:atomic]
        end
        return mass / af_sum
      end
      raise "Mix '#{@name}' has no compounds" if @compounds.empty?
      return @average_atomic_mass unless @average_atomic_mass.nil? # just to save some time instead of recalculating trees
      @average_atomic_mass = 0
      self.normalize_fractions
      @compounds.values.each do |c|
        @average_atomic_mass += c.fraction(:atomic) * c.average_atomic_mass
        af_sum += c.fraction(:atomic)
      end
      return @average_atomic_mass /= af_sum
    end

    def atomic_density
      # 1 u = 1.660 538 782(83) × 10-24 [g]
      # Na = 6.02214179(30) × 10+23 [mol-1] (Avogadro constant)
      # Na * u = 12
      self.density.to_f / (self.average_atomic_mass.to_f * 1.660538782) # [nuclides/barn/cm]
    end

    def nuclides
      fractions = {}
      self.normalize_fractions
      @compounds.values.each do |c|
        [:atomic, :mass].each do |type|
          if c.is_mix?
            if c.compound.natural? and @mixer.xs_key( c.compound.name, @temperature, :exact_match)
              nuc = @mixer.get(nuclide: c.compound.name)
              raise unless nuc
              fractions[nuc] ||= {}
              fractions[nuc][type] ||= 0
              fractions[nuc][type] += c.fraction(type)
            else
              c.compound.nuclides.each do |nuclide, fr|
                fractions[nuclide] ||= {}
                fractions[nuclide][type] ||= 0
                fractions[nuclide][type] += c.fraction(type) * fr[type]
              end
            end
          else
            fractions[c.compound] ||= {}
            fractions[c.compound][type] ||= 0
            fractions[c.compound][type] += c.fraction(type)
          end
        end
      end
      return fractions
    end
    
    
    def elements
      sums = {}
      self.nuclides.each_pair do |nuclide,fractions|
        sums[ nuclide.symbol ] ||= {}
        [:atomic,:mass].each do |type|
          sums[ nuclide.symbol ][type] ||= 0
          sums[ nuclide.symbol ][type] += fractions[type]
          sums[ nuclide.symbol ][:nuclides] ||= {}
          sums[ nuclide.symbol ][:nuclides][nuclide] = fractions
        end
      end
      sums.each_pair do |element_name, element_fractions|
        element_fractions[:nuclides].each_pair do |nuclide, nuclide_fractions|
          [:atomic,:mass].each do |type|
            nuclide_fractions[type] /= element_fractions[type]
          end
        end
      end
      return sums
    end
    
    def stat_compounds
      sums = {}
      self.compounds.each_pair do |name,cf|
        next if Nuclide.valid? name
        sums[ name ] ||= {}
        [:atomic,:mass].each do |type|
          sums[ name ][type] ||= 0
          sums[ name ][type] += cf.fraction type
        end
      end
      return sums
    end
    
    def fraction_of name, type= :atomic
      els = self.elements
      name = name.to_s
      if @compounds.has_key? name
        return @compounds[name].fraction(type)
      elsif els.has_key? name
        return els[ name.to_s ][type]
      else
        fr = 0
        @compounds.values.each do |cf|
          fr += cf.fraction(type) * cf.compound.fraction_of(name,type)
        end
        return fr
      end
    end
    
    
    def deep_clone
      Marshal::load(Marshal.dump(self))
    end
  end
  
  
  







  class Nuclide
    @@symbols, @@masses = {}, {}
    @@instances = {}

    def initialize id # DO NOT USE #new TO CREATE A NEW INSTANCE - USE #get!!!
      self.class.load_constants
      @metastable = false
      self.identify(id)
    end

    def self.get id # collection
      temp = Nuclide.new id
      return @@instances[temp.zaid] ||= temp # add to collection
    end
    
    # methods to add: isotopes, isotones, isobars
    def metastable?; @metastable end
    def metastate;   @metastable end
    def z; @element_number end
    def a; @mass_number end
    def symbol; @@symbols[@element_number] end
    alias :element_symbol :symbol
    def zaid
      r = '%s%03d%s' % [@element_number, @mass_number.to_i, self.metaname]
      return {'95242m'=>'95342','95244m'=>'95344'}[r] || r
    end
    def name; self.symbol + @mass_number.to_s + self.metaname end
    alias :to_s :name
    alias :inspect :name
    def ground_state; self.class.get(self.symbol + @mass_number.to_s) end
    def mass
      if @@masses.has_key? self.zaid
        return @@masses[self.zaid].to_f
      elsif self.metastable?
        return self.ground_state.mass
      else
        raise "No mass for #{self.zaid}"
      end
    end
    alias :average_atomic_mass :mass

    
    def metaname
      if @metastable
        r = 'm'  
        r += '%d' % @metastable if @metastable > 1
      end
      return r ||= ''
    end


    def self.valid? id
      self.load_constants
      case id
        when /^(\d{1,3}-)?([A-Z][a-z]*)-?\d{1,3}(m\d?)?$/ # 95-Am-244m2, Am-244m2, Am244m2
          return false if $1 and $1.to_i > 120
          return @@symbols.has_value?($2)
        when /^(\d{4,6})(m\d?)?$/ # 95244m2
          return false if $1.to_i > 120_000
          return true
      else return false end
    end
    
    
    def identify id
      id = {'95292'=>'95242m','95342'=>'95242m','95294'=>'95244m','95344'=>'95244m'}[id.to_s] || id.to_s
      case id
        when /^(\d{1,3})?[-]?([A-Z][a-z]*)[-]?(\d{1,6})(m(\d?))?$/ # 94-Pu-239, Am242m2, Pu239, Pu-239, Am-242m,
          # 1:94, 2:Pu, 3:239, 4:m2, 5:2
          raise "Error: unknown nuclide symbol: #{$2}" unless @@symbols.has_value?( $2 )
          raise "Error: nuclide number and symbol do not match: #{$1} #{$2}" unless $1.nil? or (@@symbols[ $1 ] == $2)
          @element_number = $1 || @@symbols.key($2)
          @mass_number    = $3
          @metastable     = ($5 || 1).to_i unless $4.nil?

        when /^([a-zA-Z]{1,3})$/ # Pu, not plutonium
          raise "Error: unknown nuclide symbol: #{$1}" unless @@symbols.has_value? $1 
          @element_number = @@symbols.key $1
          @mass_number    = nil# '000'

        when /^(\d{1,3})$/ # 94
          raise "Error: bad element number: #{$1}" if $1.to_i > 120
          @element_number = $1
          @mass_number   = nil#'000'

        when /^(\d{1,3})(\d{3})(m(\d*))?$/ # 94239, 95242m
          @element_number = $1
          @mass_number    = $2
          @metastable     = ($4 || 1).to_i unless $3.nil?
      else
        raise "#{id} is not a valid nuclide id"
      end
      return true
    end


    def self.load_constants
      return unless @@symbols.empty?
      ELEMENT_SYMBOLS.split.each_slice(3) { |a| @@symbols[ a[0] ] = a[1] }
      XSDIR_MASSES.split.each_slice(2) do |pair|
        zaid,mass = pair
        mass = mass.to_f * 1.008664915 # conversion of [neutron masses] to [u]
        #a = zaid.to_i / 1000; z = zaid.to_i - a * 1000
        #zaid = {'95292'=>'95242m','95342'=>'95242m','95294'=>'95244m','95344'=>'95244m'}[zaid] || zaid
        @@masses[ zaid ] = mass
      end
    end
  
    # constants of Nuclide class

    ELEMENT_SYMBOLS = "
0	  n 	neutron
1	  H 	hydrogen
2	  He	helium
3	  Li	lithium
4	  Be	beryllium
5	  B 	boron
6	  C 	carbon
7	  N 	nitrogen
8	  O 	oxygen
9	  F 	fluorine
10	Ne	neon
11	Na	sodium
12	Mg	magnesium
13	Al	aluminum
14	Si	silicon
15	P 	phosphorus
16	S 	sulfur
17	Cl	chlorine
18	Ar	argon
19	K 	potassium
20	Ca	calcium
21	Sc	scandium
22	Ti	titanium
23	V 	vanadium
24	Cr	chromium
25	Mn	manganese
26	Fe	iron
27	Co	cobalt
28	Ni	nickel
29	Cu	copper
30	Zn	zinc
31	Ga	gallium
32	Ge	germanium
33	As	arsenic
34	Se	selenium
35	Br	bromine
36	Kr	krypton
37	Rb	rubidium
38	Sr	strontium
39	Y 	yttrium
40	Zr	zirconium
41	Nb	niobium
42	Mo	molybdenum
43	Tc	technetium
44	Ru	ruthenium
45	Rh	rhodium
46	Pd	palladium
47	Ag	silver
48	Cd	cadmium
49	In	indium
50	Sn	tin
51	Sb	antimony
52	Te	tellurium
53	I 	iodine
54	Xe	xenon
55	Cs	cesium
56	Ba	barium
57	La	lanthanum
58	Ce	cerium
59	Pr	praseodymium
60	Nd	neodymium
61	Pm	promethium
62	Sm	samarium
63	Eu	europium
64	Gd	gadolinium
65	Tb	terbium
66	Dy	dysprosium
67	Ho	holmium
68	Er	erbium
69	Tm	thulium
70	Yb	ytterbium
71	Lu	lutetium
72	Hf	hafnium
73	Ta	tantalum
74	W 	tungsten
75	Re	rhenium
76	Os	osmium
77	Ir	iridium
78	Pt	platinum
79	Au	gold
80	Hg	mercury
81	Tl	thallium
82	Pb	lead
83	Bi	bismuth
84	Po	polonium
85	At	astatine
86	Rn	radon
87	Fr	francium
88	Ra	radium
89	Ac	actinium
90	Th	thorium
91	Pa	protactinium
92	U 	uranium
93	Np	neptunium
94	Pu	plutonium
95	Am	americium
96	Cm	curium
97	Bk	berkelium
98	Cf	californium
99	Es	einsteinium
100	Fm	fermium
101	Md	mendelevium
102	No	nobelium
103	Lr	lawrencium
104	Rf	rutherfordium
105	Db	dubnium
106	Sg	seaborgium
107	Bh	bohrium
108	Hs	hassium
109	Mt	meitnerium
110	Ds	darmstadtium
111	Rg	roentgenium
112	Uub	ununbium
113	Uut	ununtrium
114	Uuq	ununquadium
115	Uup	ununpentium
116	Uuh	ununhexium
117	Uus	ununseptium
118	Uuo	ununoctium
    "
    XSDIR_MASSES = "
      0001  1.000000      0001  1.000000
      1000  0.99931697    1001  0.99916732   1002  1.99679966   1003  2.99013994
      2000  3.96821760    2003  2.99012015   2004  3.96821894
      3000  6.88131188    3006  5.96344945   3007  6.95573316
      4000  8.93476310    4007  6.95665041   4009  8.93476310
      5000  10.7181562    5010  9.92692102   5011  10.9147302   5012  11.9111422
      6000  11.9078563    6012  11.8969142   6013  12.8916497   6014  13.8829473
      7000  13.8863986    7014  13.8827808   7015  14.8712507   7016  15.8686012
      8000  15.8618629    8016  15.8575105   8017  16.8531007   8018  17.8445389
                          8019  18.8403287
      9000  18.8351977    9018  17.8463008   9019  18.8351977
     10000  20.0066900    10020  19.8206954   10021  20.8134994   10022  21.8024688
     11000  22.7922764    11022  21.8054924   11023  22.7922764   11024  23.7848679
     12000  24.0962596    12023  22.7965941   12024  23.7789987   12025  24.7711965
                          12026  25.7593899   12027  26.7525321
     13000  26.7497539    13026  25.7636516   13027  26.7497539   13028  27.7415320
     14000  27.8442403    14027  26.7548758   14028  27.7365911   14029  28.7275725
                          14030  29.7162809   14031  30.7092698
     15000  30.7076818    15031  30.7076818   15032  31.6992346
     16000  31.7889394    16031  30.7134250   16032  31.6974150   16033  32.6882176
                          16034  33.6760663   16035  34.6686311   16036  35.6581061
                          16037  36.6535257
     17000  35.1481816    17034  33.6819108   17035  34.6684532   17036  35.6593217
                          17037  36.6483475   17038  37.6418469
     18000  39.6044909    18036  35.6585675   18037  36.6492133   18038  37.6366138
                          18039  38.6295910   18040  39.6190868   18041  40.6125958
                          18042  41.6025634   18043  42.5965747
     19000  38.7624279    19038  37.6429072   19039  38.6289896   19040  39.6206885
                          19041  40.6099439   19042  41.6019256   19043  42.5916623
                          19044  43.5839050   19045  44.5744654   19046  45.5671404
     20000  39.7337331    20039  38.6359403   20040  39.6192930   20041  40.6103924
                          20042  41.5981734   20043  42.5897301   20044  43.5778821
                          20045  44.5699904   20046  45.5589282   20047  46.5511840
                          20048  47.5405979   20049  48.5351202
     21000  44.5697171    21044  43.5817704   21045  44.5697171   21046  45.5603930
                          21047  46.5490640   21048  47.5403019
     22000  47.4671267    22045  44.5719122   22046  45.5578740   22047  46.5484253
                          22048  47.5360508   22049  48.5273847   22050  49.5157420
                          22051  50.5089598
     23000  50.5038596    23047  46.5515414   23048  47.5403213   23049  48.5280253
                          23050  49.5180923   23051  50.5063302   23052  51.4985487
                          23053  52.4895248   23054  53.4830182
     24000  51.5494654    24049  48.5308253   24050  49.5169887   24051  50.5071314
                          24052  51.4943176   24053  52.4858678   24054  53.4755236
                          24055  54.4688756   24056  55.4600879   24057  56.4545792
     25000  54.4661050    25051  50.5105455   25052  51.4993323   25053  52.4865032
                          25054  53.4769893   25055  54.4661050   25056  55.4583669
                          25057  56.4491598   25058  57.4422538
     26000  55.3670672    26053  52.4904863   26054  53.4762473   26055  54.4663513
                          26056  55.4544337   26057  56.4462959   26058  57.4356054
                          26059  58.4286012   26060  59.4192140   26061  60.4132731
     27000  58.4269353    27057  56.4471857   27058  57.4380612   27059  58.4269353
                          27060  59.4189615   27061  60.4090397   27062  61.4020105
                          27063  62.3929848   27064  63.3865737
     28000  58.1891568    28057  56.4506599   28058  57.4376551   28059  58.4280768
                          28060  59.4159560   28061  60.4076330   28062  61.3963541
                          28063  62.3890764   28064  63.3787972   28065  64.3723069
     29000  62.9997558    29062  61.4005562   29063  62.3890052   29064  63.3805800
                          29065  64.3700323   29066  65.3625119
     30000  64.8345785    30064  63.3799641   30066  65.3597000   30067  66.3521942
                          30068  67.3413400   30070  69.3246323   30072  71.3089747
     31000  69.1241164    31069  68.3334766   31071  70.3154273   31072  71.3084831
     32000  71.9677766    32070  69.3235670   32072  71.3042305   32073  72.2970113
                          32074  73.2861593   32076  75.2692009   32077  76.2627318
     33000  74.2779834    33072  71.3088668   33073  72.2973741   33074  73.2888865
                          33075  74.2779834   33077  76.2598575
     34000  78.2812872    34074  73.2874465   34076  75.2670311   34077  76.2591351
                          34078  77.2479619   34079  78.2405510   34080  79.2300000
                          34082  81.2129957
     35000  79.2171183    35079  78.2403907   35081  80.2211807   35082  81.2130927
     36000  83.0801410    36078  77.2510122   36080  79.2298574   36082  81.2098079
                          36083  82.2018631   36084  83.1906659   36085  84.1830853
                          36086  85.1725963
                          36087  86.1667212   36088  87.1592123   36089  88.1537834
     37000  84.7335543    37085  84.1823557   37086  85.1731409   37087  86.1625913
                          37088  87.1561111   37089  88.1484736
     38000  86.8639767    38084  83.1925676   38086  85.1712599   38087  86.1622897
                          38088  87.1504623   38089  88.1436933   38090  89.9077376
                          38091  90.1292427   38092  91.1214691
     39000  88.1421034    39086  85.1768370   39087  86.1642711   39088  87.1543179
                          39089  88.1421034   39090  89.1348052   39091  90.1263654
                          39092  91.1194042   39093  92.1114430
     40000  90.4399916    40088  87.1550345   40089  88.1451183   40090  89.1323785
                          40091  90.1247212   40092  91.1155311   40093  92.1083638
                          40094  93.0996148   40095  94.0927364   40096  95.0843769
                          40097  96.0784384
     41000  92.1082665    41091  90.1260552   41092  91.1176657   41093  92.1082665
                          41094  93.1005742   41095  94.0915392   41096  95.0842028
                          41097  96.0756094   41098  97.0692333   41100  99.0558700
     42000  95.1071951    42090  89.1415317   42091  90.1307745   42092  91.1172862
                          42093  92.1086974   42094  93.0983971   42095  94.0905541
                          42096  95.0808110   42097  96.0735511   42098  97.0643527
                          42099  98.0580462   42100  99.0492233   42101  100.043478
     43000  96.0740000    43097  96.0738920   43098  97.0661450   43099  98.0566017
     44000  100.201505    44096  95.0837047   44098  97.0642330   44099  98.0562891
                          44100  99.0459938   44101  100.038754   44102  101.028942
                          44103  102.022308   44104  103.012832   44105  104.006531
                          44106  104.997526
     45000  102.021496    45103  102.021496   45105  104.004501   45117  115.54464
     46000  105.501169    46102  101.030189   46104  103.011449   46105  104.003898
                          46106  104.993721   46107  105.986757   46108  106.976947
                          46109  107.970393   46110  108.961014
                          46112  110.945967   46119  117.52551
     47000  106.941511    47106  104.996877   47107  105.986726   47108  106.978989
                          47109  107.969211   47110  108.961963   47111  109.952561
     48000  111.445883    48106  104.996670   48108  106.977234   48110  108.958885
                          48111  109.951461   48112  110.941458   48113  111.934497
                          48114  112.924873   48115  113.918335   48116  114.909077
     49000  113.831545    49113  111.934160   49115  113.916798   49120  118.490609
                          49125  118.490609
     50000  117.691126    50112  110.943504   50114  112.924301   50115  113.916270
                          50116  114.906092   50117  115.898700   50118  116.888774
                          50119  117.881872   50120  118.872178   50121  119.865604
                          50122  120.856230   50123  121.849898   50124  122.840868
                          50125  123.834759   50126  124.826040
     51000  120.710837    51121  119.865195   51122  120.857944   51123  121.848409
                          51124  122.841519   51125  123.832248   51126  124.825639
                          51127  125.816716   51128  126.810357   51129  127.801751
     52000  126.492160    52120  118.873986   52122  120.855841   52123  121.848465
                          52124  122.838434   52125  123.831435   52126  124.821735
                          52127  125.815037   52128  126.805700   52129  126.905217
                          52130  128.790265   52131  129.783950   52132  130.775359
                          52134  132.761174
     53000  125.814297    53127  125.814297   53129  127.797631   53130  128.790710
                          53131  129.781575   53132  130.77483    53133  131.766058
                          53134  132.759525   53135  133.751107
     54000  130.165210    54124  122.841484   54126  124.822690   54128  126.804777
                          54129  127.797425   54130  128.787574   54131  129.780544
                          54132  130.771034   54133  131.764167   54134  132.755082
                          54135  133.748289   54136  134.739710   54137  135.735417
                          54138  136.729232
     55000  131.763724    55133  131.763724   55134  132.756390   55135  133.747064
                          55136  134.739796   55137  135.730985   55138  136.726282
     56000  136.147201    56130  128.790352   56132  130.771927   56134  132.754198
                          56135  133.746777   56136  134.737084   56137  135.729734
                          56138  136.720568   56139  137.715528   56140  138.708693
                          56141  139.703884   56142  140.697305
     57000  137.712181    57138  136.722418   57139  137.713075   57140  138.707573
                          57141  139.700450   57142  140.694961
     58000  138.911205    58136  134.739635   58138  136.721306   58140  138.703578
                          58141  139.697787   58142  140.690170   58143  141.684691
                          58144  142.677341
     59000  139.697182    59141  139.697182   59142  140.690952   59143  141.683133
                          59145  143.669613
     60000  143.003225    60142  140.688662   60143  141.682145   60144  142.673825
                          60145  143.667699   60146  144.659647   60147  145.654015
                          60148  146.646210   60150  148.632993
     61000  143.668000    61145  143.667873   61147  145.653061   61148  146.646785
                          61149  147.639048   61151  149.624710
     62000  149.068569    62144  142.675721   62147  145.652823   62148  146.644158
                          62149  147.637908   62150  148.629409   62151  149.623453
                          62152  150.614664   62153  151.608407   62154  152.599939
     63000  150.659117    63151  149.623371   63152  150.616659   63153  151.607558
                          63154  152.600702   63155  153.592026   63156  154.585267
                          63157  155.577343
     64000  155.901247    64150  148.630781   64151  149.623865   64152  150.614723
                          64153  151.608074   64154  152.598607   64155  153.591758
                          64156  154.582673   64157  155.575903   64158  156.567456
                          64159  157.561130   64160  158.553199   64161  159.547201
     65000  157.560097    65159  157.560097   65160  158.553311   65161  159.545113
     66000  161.101597    66156  154.584813   66158  156.567757   66160  158.551358
                          66161  159.544489   66162  160.535765   66163  161.529090
                          66164  162.520940
     67000  163.513487    67163  161.529093   67164  162.521990   67165  163.513487
                          67166  164.506842
     68000  165.818892    68162  160.537728   68164  162.520965   68166  164.504868
                          68167  165.498018   68168  166.489747   68170  168.475632
     69000  167.482984    69166  164.508103   69167  165.498814   69168  166.491534
                          69169  167.482984   69170  168.475967   69171  169.467999
                          69172  170.461362   69173  171.453965
     70000  171.547739    70168  166.491261   70170  168.474936   70171  169.467896
                          70172  170.459360   70173  171.452583   70174  172.444639
                          70176  174.431136
     71000  173.463668    71173  171.453297   71174  172.446101   71175  173.437942
                          71176  174.431249   71177  175.423722
     72000  176.953118    72174  172.445810   72175  173.438670   72176  174.429979
                          72177  175.423192   72178  176.415075   72179  177.408583
                          72180  178.400720   72181  179.394658   72183  181.381871
     73000  179.393446    73179  177.408701   73180  178.401629   73181  179.393565
                          73182  180.387112   73183  181.379732   73184  182.373755
                          73186  184.361076
     74000  182.269538    74179  177.409829   74180  178.400876   74181  179.393765
                          74182  180.385182   74183  181.378593   74184  182.370704
                          74185  183.364581   74186  184.356924   74187  185.351105
                          74188  186.343832
     75000  184.607099    75184  182.372283   75185  183.364120   75186  184.357543
                          75187  185.349710   75188  186.343461
     76000  188.605524    76184  182.372249   76186  184.356404   76187  185.349707
                          76188  186.341204   76189  187.334902   76190  188.326610
                          76192  190.312436
     77000  190.564824    77188  186.344194   77189  187.335469   77190  188.328738
                          77191  189.320147   77192  190.313550   77193  191.305278
                          77194  192.298821   77195  193.291124
     78000  193.404281    78190  188.328082   78192  190.311996   78193  191.305338
                          78194  192.296430   78195  193.289932   78196  194.281501
                          78197  195.275278   78198  196.267236   78199  197.261322
     79000  195.274513    79193  191.306476   79194  192.299082   79195  193.290173
                          79196  194.283103   79197  195.274513   79198  196.267582
                          79199  197.259510   79200  198.252872
     80000  198.875905    80196  194.282373   80198  196.266121   80199  197.259028
                          80200  198.250484   80201  199.243853   80202  200.235600
                          80203  201.229221   80204  202.221244
     81000  202.627565    81202  200.237052   81203  201.228698   81204  202.221614
                          81205  203.213582
     82000  205.436797    82202  200.237105   82203  201.229735   82204  202.220801
                          82205  203.213637   82206  204.205028   82207  205.197857
                          82208  206.190015   82209  207.185826   82210  208.180307
     83000  207.185140    83205  203.216519   83206  204.209027   83207  205.200409
                          83208  206.193080   83209  207.185140   83210  208.180239
     84000  207.205000    84205  203.220277   84206  204.210993   84207  205.203506
                          84208  206.194571   84209  207.187155   84210  208.179002
     85000  208.196000    85206  204.217074   85208  206.199876   85209  207.190866
                          85210  208.183240   85215  213.151698
     86000  220.111000    86211  209.178074   86218  216.132812   86219  217.128077
                          86220  218.121379   86222  220.110331
     87000  221.084000    87221  219.115626   87223  221.103883
     88000  224.084000    88222  220.108142   88223  221.102660   88224  222.095759
                          88225  223.090542   88226  224.083734   88227  225.078880
     89000  225.050000    89223  221.103283   89224  222.097253   89225  223.090162
                          89226  224.084416   89227  225.077468   89228  226.072117
     90000  230.044730    90227  225.077420   90228  226.069853   90229  227.064261
                          90230  228.057030   90231  229.051583   90232  230.044730
                          90233  231.039636   90234  232.033047
     91000  229.051000    91229  227.064591   91230  228.058424   91231  229.051168
                          91232  230.045257   91233  231.038311
     92000  235.984128    92229  227.065987   92230  228.057824   92231  229.051575
                          92232  230.043834   92233  231.037704   92234  232.030420
                          92235  233.024781   92236  234.017816   92237  235.012360
                          92238  236.005810   92239  237.000695   92240  237.994383
                          92241  238.989504
     93000  235.012000    93235  233.024913   93236  234.018805   93237  235.011808
                          93238  236.005967   93239  236.999350
     94000  241.968000    94236  234.018298   94237  235.012043   94238  236.004592
                          94239  236.998582   94240  237.991628   94241  238.986050
                          94242  239.979334   94243  240.973976   94244  241.967567
                          94245  242.962487   94246  243.956336
     95000  240.912000    95240  237.993096   95241  238.986027   95242  239.980134
                          95243  240.973357   95244  241.967648
     96000  244.878000    96241  238.986844   96242  239.979426   96243  240.973367
                          96244  241.966128   96245  242.960254   96246  243.953380
                          96247  244.947892   96248  245.941280
     97000  244.878000    97245  242.961116   97246  243.954817   97247  244.947844
                          97248  245.942011   97249  246.935305
     98000  248.844000    98249  246.935173   98250  247.928122   98251  248.922684
                          98252  249.916116
     99000  251.818000    99240  238.006611   99241  238.997765   99242  239.990202
                          99243  240.981544   99244  241.974280   99245  242.966035
                          99246  243.959078   99247  244.951167   99248  245.944369
                          99249  246.936717   99250  247.930357   99251  248.923084
                          99252  249.917457   99253  250.910696   99254  251.905276
                          99255  252.898917   99256  253.893623   99257  254.887399
    100000  254.792000
    "
  end
  
  # constants of MaterialMixer class
  
  LIBRARY_COMPOSITIONS = "
SS316              C        0.03          density=-7.6
                   Mn       2.0  
                   Si       0.75    
                   P        0.045
                   S        0.03 
                   Cr      18.0  
                   Mo       3.0  
                   Ni      14.0  
                   Nb       0.1  
                   Fe       balance
                   
15/15Ti            B        2.5584E-04    density=-7.76
                   C        0.461   
                   Si       1.182   
                   P        0.054   
                   Ti       0.462   
                   Cr      15.958   
                   Mn       1.510   
                   Ni      14.137   
                   Mo       0.692
                   Fe       balance
  "
  
  NATURAL_COMPOSITIONS = "
# source: http://atom.kaeri.re.kr/ton/
  
# Hydrogen   Z=1
H           H-1        99.985 
            H-2         0.015 

# Helium     Z=2
He         He-3      0.000137 
           He-4     99.999863 

# Lithium    Z=3
Li         Li-6           7.5 
           Li-7          92.5 

# Beryllium  Z=4
Be         Be-9           100 

# Boron      Z=5
B          B-10          19.9 
           B-11          80.1 

# Carbon     Z=6
C          C-12         98.89 
           C-13          1.11 

# Nitrogen   Z=7
N          N-14        99.634 
           N-15         0.366 

# Oxygen     Z=8
O          O-16        99.762 
           O-17         0.038 
           O-18         0.200 

# Fluorine   Z=9
F          F-19           100 

# Neon       Z=10
Ne        Ne-20         90.48 
          Ne-21          0.27 
          Ne-22          9.25 

# Sodium     Z=11
Na        Na-23           100 

# Magnesium  Z=12
Mg        Mg-24         78.99 
          Mg-25         10.00 
          Mg-26         11.01 

# Aluminum   Z=13
Al        Al-27           100 

# Silicon    Z=14
Si        Si-28         92.23 
          Si-29          4.67 
          Si-30          3.10 

# Phosphorus Z=15
P          P-31           100 

# Sulfur     Z=16
S          S-32         95.02 
           S-33          0.75 
           S-34          4.21 
           S-36          0.02 

# Chlorine   Z=17
Cl        Cl-35         75.77 
          Cl-37         24.23 

# Argon      Z=18
Ar        Ar-36        0.3365 
          Ar-38        0.0632 
          Ar-40       99.6003 

# Potassium  Z=19
K          K-39       93.2581 
           K-40        0.0117 # half-life = 1.277E+9
           K-41        6.7302 

# Calcium    Z=20
Ca        Ca-40        96.941 
          Ca-42         0.647 
          Ca-43         0.135 
          Ca-44         2.086 
          Ca-46         0.004 
          Ca-48         0.187 # half-life = 6E+18 Y

# Scandium   Z=21
Sc        Sc-45           100 

# Titanium   Z=22
Ti        Ti-46          8.25 
          Ti-47          7.44 
          Ti-48         73.72 
          Ti-49          5.41 
          Ti-50          5.18 

# Vanadium   Z=23
V          V-50         0.250 # half-life = 1.4E+17
           V-51        99.750 

# Chromium   Z=24
Cr        Cr-50         4.345 # half-life = 1.8E+17
          Cr-52        83.789 
          Cr-53         9.501 
          Cr-54         2.365 

# Manganese  Z=25
Mn        Mn-55           100 

# Iron       Z=26
Fe        Fe-54         5.845 
          Fe-56        91.754 
          Fe-57         2.119 
          Fe-58         0.282 

# Cobalt     Z=27
Co        Co-59           100 

# Nickel     Z=28
Ni        Ni-58        68.077 
          Ni-60        26.223 
          Ni-61         1.140 
          Ni-62         3.634 
          Ni-64         0.926 

# Copper     Z=29
Cu        Cu-63         69.17 
          Cu-65         30.83 

# Zinc       Z=30
Zn        Zn-64          48.6 
          Zn-66          27.9 
          Zn-67           4.1 
          Zn-68          18.8 
          Zn-70           0.6 # half-life = 5E+14 Y

# Gallium    Z=31
Ga        Ga-69        60.108 
          Ga-71        39.892 

# Germanium  Z=32
Ge        Ge-70         21.23 
          Ge-72         27.66 
          Ge-73          7.73 
          Ge-74         35.94 
          Ge-76          7.44 

# Arsenic    Z=33
As        As-75           100 

# Selenium   Z=34
Se        Se-74          0.89 
          Se-76          9.36 
          Se-77          7.63 
          Se-78         23.78 
          Se-80         49.61 
          Se-82          8.73 # half-life = 1.08E+20

# Bromine    Z=35
Br        Br-79         50.69 
          Br-81         49.31 

# Krypton    Z=36
Kr        Kr-78          0.35 # half-life = 2.0E+21
          Kr-80          2.25 
          Kr-82          11.6 
          Kr-83          11.5 
          Kr-84          57.0 
          Kr-86          17.3 

# Rubidium   Z=37
Rb        Rb-85        72.165 
          Rb-87        27.835 # half-life = 4.75E10

# Strontium  Z=38
Sr        Sr-84          0.56 
          Sr-86          9.86 
          Sr-87          7.00 
          Sr-88         82.58 

# Yttrium    Z=39
Y          Y-89           100 

# Zirconium  Z=40
Zr        Zr-90         51.45 
          Zr-91         11.22 
          Zr-92         17.15 
          Zr-94         17.38 
          Zr-96          2.80 # half-life = 3.9E19 Y

# Niobium    Z=41
Nb        Nb-93           100 

# Molybdenum Z=42
Mo        Mo-92         14.84 
          Mo-94          9.25 
          Mo-95         15.92 
          Mo-96         16.68 
          Mo-97          9.55 
          Mo-98         24.13 
         Mo-100          9.63 # half-life = 1.2E19 Y

# Ruthenium  Z=44
Ru        Ru-96          5.52 
          Ru-98          1.88 
          Ru-99          12.7 
         Ru-100          12.6 
         Ru-101          17.0 
         Ru-102          31.6 
         Ru-104          18.7 

# Rhodium    Z=45
Rh       Rh-103           100 

# Palladium  Z=46
Pd       Pd-102          1.02 
         Pd-104         11.14 
         Pd-105         22.33 
         Pd-106         27.33 
         Pd-108         26.46 
         Pd-110         11.72 

# Silver     Z=47
Ag       Ag-107        51.839 
         Ag-109        48.161 

# Cadmium    Z=48
Cd       Cd-106          1.25 
         Cd-108          0.89 
         Cd-110         12.49 
         Cd-111         12.80 
         Cd-112         24.13 
         Cd-113         12.22 # half-life = 9.3E+15
         Cd-114         28.73 # half-life = STABL
         Cd-116          7.49 

# Indium     Z=49
In       In-113          4.29 
         In-115         95.71 # half-life = 4.41E+14

# Tin        Z=50
Sn       Sn-112          0.97 
         Sn-114          0.65 
         Sn-115          0.34 
         Sn-116         14.54 
         Sn-117          7.68 
         Sn-118         24.22 
         Sn-119          8.58 
         Sn-120         32.59 
         Sn-122          4.63 
         Sn-124          5.79 

# Antimony   Z=51
Sb       Sb-121         57.21 
         Sb-123         42.79 

# Tellurium  Z=52
Te       Te-120         0.096 
         Te-122         2.603 
         Te-123         0.908 # half-life = 1E+13 Y
         Te-124         4.816 
         Te-125         7.139 
         Te-126        18.952 
         Te-128        31.687 # half-life = 7.7E24 Y
         Te-130        33.799 # half-life = 2.7E+21

# Iodine     Z=53
I         I-127           100 

# Xenon      Z=54
Xe       Xe-124          0.10 
         Xe-126          0.09 
         Xe-128          1.91 
         Xe-129          26.4 
         Xe-130           4.1 
         Xe-131          21.2 
         Xe-132          26.9 
         Xe-134          10.4 
         Xe-136           8.9 # half-life = 9.3E+19

# Cesium     Z=55
Cs       Cs-133           100 

# Barium     Z=56
Ba       Ba-130         0.106 
         Ba-132         0.101 
         Ba-134         2.417 
         Ba-135         6.592 
         Ba-136         7.854 
         Ba-137         11.23 
         Ba-138         71.70 

# Lanthanum  Z=57
La       La-138        0.0902 # half-life = 1.05E+11
         La-139       99.9098 

# Cerium     Z=58
Ce       Ce-136          0.19 
         Ce-138          0.25 
         Ce-140         88.48 
         Ce-142         11.08 # half-life = 5E+16 Y

# Praseodymium Z=59
Pr       Pr-141           100 

# Neodymium  Z=60
Nd       Nd-142         27.13 
         Nd-143         12.18 
         Nd-144         23.80 # half-life = 2.29E+15
         Nd-145          8.30 
         Nd-146         17.19 
         Nd-148          5.76 
         Nd-150          5.64 # half-life = 1.1E19 Y

# Samarium   Z=62
Sm       Sm-144           3.1 
         Sm-147          15.0 # half-life = 1.06E+11
         Sm-148          11.3 # half-life = 7E+15 Y
         Sm-149          13.8 # half-life = 2E+15 Y
         Sm-150           7.4 
         Sm-152          26.7 
         Sm-154          22.7 

# Europium   Z=63
Eu       Eu-151          47.8 
         Eu-153          52.2 

# Gadolinium Z=64
Gd       Gd-152          0.20 # half-life = 1.08E14
         Gd-154          2.18 
         Gd-155         14.80 
         Gd-156         20.47 
         Gd-157         15.65 
         Gd-158         24.84 
         Gd-160         21.86 

# Terbium    Z=65
Tb       Tb-159           100 

# Dysprosium Z=66
Dy       Dy-156          0.06 
         Dy-158          0.10 
         Dy-160          2.34 
         Dy-161          18.9 
         Dy-162          25.5 
         Dy-163          24.9 
         Dy-164          28.2 

# Holmium    Z=67
Ho       Ho-165           100 

# Erbium     Z=68
Er       Er-162          0.14 
         Er-164          1.61 
         Er-166          33.6 
         Er-167         22.95 
         Er-168          26.8 
         Er-170          14.9 

# Thulium    Z=69
Tm       Tm-169           100 

# Ytterbium  Z=70
Yb       Yb-168          0.13 
         Yb-170          3.05 
         Yb-171          14.3 
         Yb-172          21.9 
         Yb-173         16.12 
         Yb-174          31.8 
         Yb-176          12.7 

# Lutetium   Z=71
Lu       Lu-175         97.41 
         Lu-176          2.59 # half-life = 3.73E10

# Hafnium    Z=72
Hf       Hf-174         0.162 # half-life = 2.0E15 Y
         Hf-176         5.206 
         Hf-177        18.606 
         Hf-178        27.297 
         Hf-179        13.629 
         Hf-180        35.100 

# Tantalum   Z=73
Ta       Ta-180m        0.012 # half-life = > 1.2E+5
         Ta-181        99.988 

# Tungsten   Z=74
W         W-180         0.120 
          W-182        26.498 
          W-183        14.314 # half-life = 1.1E+17
          W-184        30.642 # half-life = 3E+17 Y
          W-186        28.426 

# Rhenium    Z=75
Re       Re-185         37.40 
         Re-187         62.60 # half-life = 4.35E10

# Osmium     Z=76
Os       Os-184         0.020 # half-life = 5.6E13 Y
         Os-186          1.58 # half-life = 2.0E+15
         Os-187           1.6 
         Os-188          13.3 
         Os-189          16.1 
         Os-190          26.4 
         Os-192          41.0 

# Iridium    Z=77
Ir       Ir-191          37.3 
         Ir-193          62.7 

# Platinum   Z=78
Pt       Pt-190          0.01 # half-life = 6.5E11 Y
         Pt-192          0.79 
         Pt-194          32.9 
         Pt-195          33.8 
         Pt-196          25.3 
         Pt-198           7.2 

# Gold       Z=79
Au       Au-197           100 

# Mercury    Z=80
Hg       Hg-196          0.15 
         Hg-198          9.97 
         Hg-199         16.87 
         Hg-200         23.10 
         Hg-201         13.18 
         Hg-202         29.86 
         Hg-204          6.87 

# Thallium   Z=81
Tl       Tl-203        29.524 
         Tl-205        70.476 

# Lead       Z=82
Pb       Pb-204           1.4 # half-life = 1.4E+17
         Pb-206          24.1 
         Pb-207          22.1 
         Pb-208          52.4 

# Bismuth    Z=83
Bi       Bi-209           100 

# Thorium    Z=90
Th       Th-232           100 # half-life = 1.405E10

# Uranium    Z=92
U         U-234        0.0055 # half-life = 2.455E+5
          U-235         0.720 # half-life = 703.8E+6
          U-238       99.2745 # half-life = 4.468E+9
  "
  
  NATURAL_COMPOSITIONS_OLD = "
H	1	99.985
H	2	0.015
He	3	0.000137
He	4	99.999863
Li	6	7.5
Li	7	92.5
Be	9	100
B	10	19.9
B	11	80.1
C	12	98.89
C	13	1.11
N	14	99.634
N	15	0.366
O	16	99.762
O	17	0.038
O	18	0.200
F	19	100
Ne	20	90.48
Ne	21	0.27
Ne	22	9.25
Na	23	100
Mg	24	78.99
Mg	25	10.00
Mg	26	11.01
Al	27	100
Si	28	92.23
Si	29	4.67
Si	30	3.10
P	31	100
S	32	95.02
S	33	0.75
S	34	4.21
S	36	0.02
Cl	35	75.77
Cl	37	24.23
Ar	36	0.3365
Ar	38	0.0632
Ar	40	99.6003
K	39	93.2581
K	40	0.0117	1.277E+9
K	41	6.7302
Ca	40	96.941
Ca	42	0.647
Ca	43	0.135
Ca	44	2.086
Ca	46	0.004
Ca	48	0.187	6E+18 Y
Sc	45	100
Ti	46	8.25
Ti	47	7.44
Ti	48	73.72
Ti	49	5.41
Ti	50	5.18
V	50	0.250	1.4E+17
V	51	99.750
Cr	50	4.345	1.8E+17
Cr	52	83.789
Cr	53	9.501
Cr	54	2.365
Mn	55	100
Fe	54	5.845
Fe	56	91.754
Fe	57	2.119
Fe	58	0.282
Co	59	100
Ni	58	68.077
Ni	60	26.223
Ni	61	1.140
Ni	62	3.634
Ni	64	0.926
Cu	63	69.17
Cu	65	30.83
Zn	64	48.6
Zn	66	27.9
Zn	67	4.1
Zn	68	18.8
Zn	70	0.6	5E+14 Y
Ga	69	60.108
Ga	71	39.892
Ge	70	21.23
Ge	72	27.66
Ge	73	7.73
Ge	74	35.94
Ge	76	7.44
As	75	100
Se	74	0.89
Se	76	9.36
Se	77	7.63
Se	78	23.78
Se	80	49.61
Se	82	8.73	1.08E+20
Br	79	50.69
Br	81	49.31
Kr	78	0.35	2.0E+21
Kr	80	2.25
Kr	82	11.6
Kr	83	11.5
Kr	84	57.0
Kr	86	17.3
Rb	85	72.165
Rb	87	27.835	4.75E10
Sr	84	0.56
Sr	86	9.86
Sr	87	7.00
Sr	88	82.58
Y	89	100
Zr	90	51.45
Zr	91	11.22
Zr	92	17.15
Zr	94	17.38
Zr	96	2.80	3.9E19 Y
Nb	93	100
Mo	92	14.84
Mo	94	9.25
Mo	95	15.92
Mo	96	16.68
Mo	97	9.55
Mo	98	24.13
Mo	100	9.63	1.2E19 Y
Ru	96	5.52
Ru	98	1.88
Ru	99	12.7
Ru	100	12.6
Ru	101	17.0
Ru	102	31.6
Ru	104	18.7
Rh	103	100
Pd	102	1.02
Pd	104	11.14
Pd	105	22.33
Pd	106	27.33
Pd	108	26.46
Pd	110	11.72
Ag	107	51.839
Ag	109	48.161
Cd	106	1.25
Cd	108	0.89
Cd	110	12.49
Cd	111	12.80
Cd	112	24.13
Cd	113	12.22	9.3E+15
Cd	114	28.73	STABL
Cd	116	7.49
In	113	4.29
In	115	95.71	4.41E+14
Sn	112	0.97
Sn	114	0.65
Sn	115	0.34
Sn	116	14.54
Sn	117	7.68
Sn	118	24.22
Sn	119	8.58
Sn	120	32.59
Sn	122	4.63
Sn	124	5.79
Sb	121	57.21
Sb	123	42.79
Te	120	0.096
Te	122	2.603
Te	123	0.908	1E+13 Y
Te	124	4.816
Te	125	7.139
Te	126	18.952
Te	128	31.687	7.7E24 Y
Te	130	33.799	2.7E+21
I	127	100
Xe	124	0.10
Xe	126	0.09
Xe	128	1.91
Xe	129	26.4
Xe	130	4.1
Xe	131	21.2
Xe	132	26.9
Xe	134	10.4
Xe	136	8.9	9.3E+19
Cs	133	100
Ba	130	0.106
Ba	132	0.101
Ba	134	2.417
Ba	135	6.592
Ba	136	7.854
Ba	137	11.23
Ba	138	71.70
La	138	0.0902	1.05E+11
La	139	99.9098
Ce	136	0.19
Ce	138	0.25
Ce	140	88.48
Ce	142	11.08	5E+16 Y
Pr	141	100
Nd	142	27.13
Nd	143	12.18
Nd	144	23.80	2.29E+15
Nd	145	8.30
Nd	146	17.19
Nd	148	5.76
Nd	150	5.64	1.1E19 Y
Sm	144	3.1
Sm	147	15.0	1.06E+11
Sm	148	11.3	7E+15 Y
Sm	149	13.8	2E+15 Y
Sm	150	7.4
Sm	152	26.7
Sm	154	22.7
Eu	151	47.8
Eu	153	52.2
Gd	152	0.20	1.08E14
Gd	154	2.18
Gd	155	14.80
Gd	156	20.47
Gd	157	15.65
Gd	158	24.84
Gd	160	21.86
Tb	159	100
Dy	156	0.06
Dy	158	0.10
Dy	160	2.34
Dy	161	18.9
Dy	162	25.5
Dy	163	24.9
Dy	164	28.2
Ho	165	100
Er	162	0.14
Er	164	1.61
Er	166	33.6
Er	167	22.95
Er	168	26.8
Er	170	14.9
Tm	169	100
Yb	168	0.13
Yb	170	3.05
Yb	171	14.3
Yb	172	21.9
Yb	173	16.12
Yb	174	31.8
Yb	176	12.7
Lu	175	97.41
Lu	176	2.59	3.73E10
Hf	174	0.162	2.0E15 Y
Hf	176	5.206
Hf	177	18.606
Hf	178	27.297
Hf	179	13.629
Hf	180	35.100
Ta	180m	0.012	> 1.2E+5
Ta	181	99.988
W	180	0.120
W	182	26.498
W	183	14.314	1.1E+17
W	184	30.642	3E+17 Y
W	186	28.426
Re	185	37.40
Re	187	62.60	4.35E10
Os	184	0.020	5.6E13 Y
Os	186	1.58	2.0E+15
Os	187	1.6
Os	188	13.3
Os	189	16.1
Os	190	26.4
Os	192	41.0
Ir	191	37.3
Ir	193	62.7
Pt	190	0.01	6.5E11 Y
Pt	192	0.79
Pt	194	32.9
Pt	195	33.8
Pt	196	25.3
Pt	198	7.2
Au	197	100
Hg	196	0.15
Hg	198	9.97
Hg	199	16.87
Hg	200	23.10
Hg	201	13.18
Hg	202	29.86
Hg	204	6.87
Tl	203	29.524
Tl	205	70.476
Pb	204	1.4	1.4E+17
Pb	206	24.1
Pb	207	22.1
Pb	208	52.4
Bi	209	100
Th	232	100	1.405E10
U	234	0.0055	2.455E+5
U	235	0.720	703.8E+6
U	238	99.2745	4.468E+9
  "
end


# names = Hash[ names.split.each_slice(3).collect { |abc| [abc[1], [abc.first, abc.last]] } ]
# comp = {}
# nat.split("\n").each do |line|
#   words = line.split
#   comp[name = words.shift] ||= {}
#   comp[name][name + '-' + words.shift] = words.shift + (words.empty? ? '' : ' # half-life = ' + words.join(' '))
# end
# 
# comp.each_pair do |name,nuclides|
#   print "\n# %-10s Z=%s\n%s" % [names[name].last.capitalize, names[name].first, name]
#   l = name.length
#   nuclides.each_pair do |n,fr|
#     fr,c = fr.split(' # ')
#     puts "%#{15-l}s  %12s %s" % [n,fr,c ? '# '+c : '']
#     l = 0
#   end 
# end

