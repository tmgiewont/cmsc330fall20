require_relative "graph.rb"

class Synsets
    
    def initialize
        @s_hash = Hash.new(nil)
    end

    def load(synsets_file)
        i = 1
        invalid = []
        ids = []
        if synsets_file =~ /[\w_\-\/.]+/
            File.readlines(synsets_file).each do |line|
                if(line.match(/^id:\s(\d+)\ssynset:\s([\w_\-\/.']+)(,([\w_\-\/.']+))*$/) == nil || @s_hash.include?($1.to_i) == true || ids.include?($1.to_i)) 
                    invalid.push(i)
                else
                    ids.push($1.to_i)
                end
                i += 1
            end

            if(invalid.length() == 0)
                File.readlines(synsets_file).each do |line|
                    if line =~ /^id:\s(\d+)\ssynset:\s([\w_\-\/.']+)(,([\w_\-\/.']+))*$/
                        addSet($1.to_i, line.split(": ")[2].strip.split(","))
                    end
                end
                return nil
            end
        end
        return invalid
    end

    def addSet(synset_id, nouns)
        if(synset_id < 0) || @s_hash.has_key?(synset_id) == true || nouns.length() == 0
            return false
        end
        @s_hash[synset_id] = nouns
        return true

    end

    def lookup(synset_id)
        result = @s_hash[synset_id]
        if(result == nil)
            return []
        end
        result
    end

    def findSynsets(to_find)
        if to_find.is_a?(String)
            return findhelper(to_find)
        end
        if to_find.is_a?(Array)
            a = Hash.new
            for key in to_find
                a[key] = findhelper(key)
            end
            return a
        end
        return nil
    end
    
    def findhelper(to_find)
        a = []
        @s_hash.each do |key, value|
            if @s_hash[key].include?(to_find)
                a.push(key)
            end
        end
        return a
    end

end

class Hypernyms
    def initialize
        @graph = Graph.new
        @vertices = []
    end

    def load(hypernyms_file)
        i = 1
        invalid = []
        File.readlines(hypernyms_file).each do |line|
            if(line.match(/^from:\s(\d+)\sto:\s\d+(,[\d]+)*$/) == nil) 
                invalid.push(i)
            end
            i += 1
        end
        if(invalid.length() == 0)
            File.readlines(hypernyms_file).each do |line|
                for x in line.split(": ")[2].strip.split(",").map(&:to_i)
                    addHypernym(line[(/\d+/)].to_i, x)
                end
            end
            return nil
        end
        invalid = invalid.uniq
        invalid = invalid.sort
        return invalid
    end

    def poop()
        print("#{@graph.edges}\n")
    end

    def addHypernym(source, destination)
        if(source < 0 || destination <0 || source == destination)
            return false
        end
        if(!@vertices.include? source)
            @vertices.push(source)
            @graph.addVertex(source)
        end
        if(!@vertices.include? destination)
            @vertices.push(destination)
            @graph.addVertex(destination)
        end
        if(!@graph.hasEdge?(source,destination))
            @graph.addEdge(source,destination)
        end
        return true
    end

    def lca(id1, id2)
        if (!@vertices.include? id1) || (!@vertices.include? id2)
            return nil
        end
        bf1 = @graph.bfs(id1)
        bf2 = @graph.bfs(id2)
        commons = bf1.keys & bf2.keys
        if commons.length() == 0
            return []
        end

        mins = []
        minlen = -1
        commons.each do |x|
            len = bf1[x] + bf2[x]
            if(len < minlen || minlen == -1)
                mins = [x]
                minlen = len
            elsif len == minlen
                mins.push(len)
            end
        end
        return mins
    end
end

class CommandParser
    def initialize
        @synsets = Synsets.new
        @hypernyms = Hypernyms.new
        @results = Hash.new
        @id1 = []
    end

    def parse(command)
        @results = Hash.new
        if command =~ /^\s*([a-z]+)/
            if $1 == "load"
                @results[:recognized_command] = :load
                if command =~ /^\s*load\s+([\w_\-\/\.]+)\s+([\w_\-\/\.]+)\s*$/
                    file1 = $1
                    file2 = $2
                    file1id = []
                    synset = true
                    hypernym = true
                    File.readlines(file1).each do |line|
                        if(line.match(/^id:\s(\d+)\ssynset:\s([\w_\-\/\.']+)(,([\w_\-\/\.']+))*$/) == nil || !@synsets.lookup($1.to_i).empty? || file1id.include?($1.to_i)) 
                            synset = false
                        else
                            file1id.push($1.to_i)
                        end
                    end
                    if(synset != false)
                        
                        File.readlines(file2).each do |line|
                            if(line.match(/^from:\s(\d+)\sto:\s\d+(,[\d]+)*$/) == nil) 
                                puts "hyp"
                                hypernym = false
                            elsif @synsets.lookup($1.to_i).empty? && !file1id.include?($1.to_i)
                                hypernym = false
                            else
                                for x in line.split(": ")[2].strip.split(",").map(&:to_i)
                                    if !(@synsets.lookup($1.to_i) || file1id.include?(x))
                                        hypernym = false
                                    end
                                end
                            end
                        end
                    end

                    if(synset == false || hypernym == false)
                        @results[:result] = false
                        return @results
                    else
                        @id1.push(*file1id)
                        @synsets.load(file1)
                        @hypernyms.load(file2)
                        @results[:result] = true
                    end
                else
                    @results[:result] = :error
                end
            elsif $1 == "lookup"
                @results[:recognized_command] = :lookup
                if command =~ /^\s*lookup\s+(\d)\s*$/
                    @results[:result] = @synsets.lookup($1.to_i)
                else
                    @results[:result] = :error
                end
            elsif $1 == "find"
                @results[:recognized_command] = :find
                if command =~ /^\s*find\s+([\w_\-\/.']+)\s*$/
                    @results[:result] = @synsets.findSynsets($1)
                else
                    @results[:result] = :error
                end
            elsif $1 == "findmany"
                @results[:recognized_command] = :findmany
                if command =~ /^\s*findmany\s+([\w_\-\/.']+(,[\w_\-\/.']+)+)\s*$/
                    @results[:result] = @synsets.findSynsets(command.split("findmany")[1].strip.split(","))
                else
                    @results[:result] = :error
                end
            elsif $1 == "lca"
                @results[:recognized_command] = :lca
                if command =~ /^\s*lca\s+(\d)\s+(\d)\s*$/
                    @results[:result] = @hypernyms.lca($1.to_i, $2.to_i)
                else
                    @results[:result] = :error
                end
            elsif $1 == "por"
                @hypernyms.poop()
            else
                @results[:recognized_command] = :invalid
            end    
        else
            @results[:recognized_command] = :invalid
        end


        return @results
    end
end
