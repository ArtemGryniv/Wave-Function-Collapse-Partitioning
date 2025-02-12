using Graphs, GraphRecipes, Plots   # for plotting graphs
using StatsBase                     # for sample
using Combinatorics                 # for combinations
using Colors                        # to access RGB colors
using DataStructures                # for using stack and queue
using BenchmarkTools                # for assesing performance


mutable struct Node
    key::Int
    degree::Int
    neighbors::Vector{Int}
    entropy::Float64
    set::Int
    partitioned::Bool
    edge_weights::Dict{Int, Float64} 
end

"Creates a random graph (as a .txt) as well as a SimpleGraph graph."
function setup(num_nodes, edge_multiplier, filename)
    if filename == ""
        next_available_filename = get_filename()
        println("Using this input file: $next_available_filename")
        #make_random_graph(next_available_filename, num_nodes, edge_multiplier)
        make_clustered_graph(next_available_filename, num_nodes, 5 , .9, .1)
        #make_cycle_graph(next_available_filename, num_nodes)
        #make_regular_graph(next_available_filename, num_nodes, degree)
        #make_dense_graph(next_available_filename, num_nodes, high_edge_prob)

    else 
        next_available_filename = filename 
    end
    edges = read_data(next_available_filename)
    edge_list = Edge.(edges)
    return SimpleGraph(edge_list), edges
end


"Read a graph from a file, where each line is a pair of node keys (e.g. 2 4)"
function read_data(filename)
    edges = Tuple{Int, Int, Float64}[]
    io = open(pwd() * "/downloaded_graphs/" * filename, "r");
    for line in eachline(io)
        x, y, w = split(line)
        t = (parse(Int, x), parse(Int, y), parse(Float64, w))
        push!(edges, t)
    end
    return edges
end


"Creates an array of Node structs as a way of representing the given graph."
function create_nodes_vector(graph, edges)
    nodes = Vector{Node}()
    weights_map = Dict{Tuple{Int, Int}, Float64}()

    for edge in edges
        x, y, weight = edge
        weights_map[(x, y)] = weight
        weights_map[(y, x)] = weight  # Assuming undirected graph
    end
    

    for key in 1:nv(graph)
        deg = length(all_neighbors(graph, key))
        neighbor_weights = Dict{Float64, Float64}()
        for neighbor in all_neighbors(graph, key)
            neighbor_weights[neighbor] = weights_map[(key, neighbor)]
        end
        new_node = Node(key, deg, all_neighbors(graph, key), 0, 0, false, neighbor_weights)
        push!(nodes, new_node) 
    end
    return nodes
end



function report_wfc(graph, edges, nodes)
    length(edges) < 63 && viewgraph(graph, nodes) 
end


############################################
pq = PriorityQueue{Node, Float64}()
sets = [Set{Node}(), Set{Node}()]
unpartitioned_vertices = []

function wfc_mc(graph, edges, temp, p, a)
    global sets
    global unpartitioned_vertices

    nodes = create_nodes_vector(graph, edges)
    unpartitioned_vertices = sort(nodes, by = x -> sum(values(x.edge_weights)), rev = true)

    push!(sets[1], unpartitioned_vertices[1])
    unpartitioned_vertices[1].partitioned = true
    unpartitioned_vertices[1].set = 1
    vertex = popfirst!(unpartitioned_vertices)

    propagate(vertex, nodes)

    while length(unpartitioned_vertices) != 0
    
        vertex = observe()
        if isnothing(vertex)
            break
        end

        collapse(vertex, nodes, temp, a)
        propagate(vertex, nodes)
        temp *= p
    end

    return sets, nodes
    
end

function observe()
    global pq
    global unpartitioned_vertices

    vertex, entropy = dequeue_pair!(pq)

    if entropy == 0
        for v in unpartitioned_vertices
            push!(sets[1], v)
        end
        return nothing
    end
    
    return vertex
end

function collapse(vertex, nodes, temp, a)
    global unpartitioned_vertices
    global sets

    set1_count = 0
    set2_count = 0
    
    #finding number of neighbors in each set
    for v in vertex.neighbors
        if nodes[v].set == 1
            set1_count += nodes[v].edge_weights[vertex.key]
        end
    end

    set2_count = vertex.entropy - set1_count

    #finding set that resulted in more cuts
    if set1_count > set2_count
        good_set = 2
    elseif set1_count < set2_count
        good_set = 1
    else
        good_set = rand(1:2)
    end

    bad_set = 3 - good_set

    #Simulated Annealing
    prob = rand()

    if (prob < exp(-a/temp))
        push!(sets[bad_set], vertex)
        vertex.set = bad_set
    else
        push!(sets[good_set], vertex)
        vertex.set = good_set
    end


    vertex.partitioned = true
    filter!(x -> x != vertex, unpartitioned_vertices)

end

function propagate(vertex, nodes)
    global pq
    global sets

    up_neighbors = [n for n in vertex.neighbors if nodes[n].partitioned == false]
    cont = false

    for n in up_neighbors
        set = nodes[nodes[n].neighbors[1]].set
        for nn in nodes[n].neighbors
            if nodes[nn].partitioned == false
                cont = true
                break
            end
            if nodes[nn].set != set
                cont = true
                break
            end
        end

        if cont
            continue
        end 

        push!(sets[3 - set], nodes[n])
        nodes[n].set = 3 - set
        nodes[n].partitioned = true
        filter!(x -> x != nodes[n], up_neighbors)
        filter!(x -> x != nodes[n], unpartitioned_vertices)
    end

    for n in up_neighbors
        nodes[n].entropy += nodes[n].edge_weights[vertex.key]
        if haskey(pq, nodes[n])
            delete!(pq, nodes[n])
        end
        enqueue!(pq, nodes[n], round(-(nodes[n].entropy), digits=0))
    end

    
    
end 
######################################################################

function calculate_cuts(edges, sets)
    map = Dict{Int, Int}()
    for (index, set) in enumerate(sets)
        for v in set
            map[v.key] = index
        end
    end
  

    cut_edges = Set{Tuple{Int, Int, Float64}}()
    num_cuts = 0
    total_weight = 0
    for edge in edges
        x, y, weight = edge
        if map[x] != map[y]
            num_cuts += 1
            if !((y, x, weight) in cut_edges || (x, y, weight) in cut_edges)
                total_weight += weight
                push!(cut_edges, (x, y, weight))
            end
        end
    end

    return num_cuts, collect(cut_edges), total_weight
end





#main function
function main(graph, edges, temp = 200, p = .95, a = 200, amt_trials = 1000)   
    max_cut_weight = 0
    best_partition = []
    best_nodes = []
    list_of_cuts = []

    for _ in 1:amt_trials
        sets, nodes = wfc_mc(graph, edges, temp, p, a)
        num_cuts, cut_edges, total_weight = calculate_cuts(edges, sets)
        push!(list_of_cuts, num_cuts)
        
        if total_weight > max_cut_weight
            max_cut_weight = total_weight
            best_partition = deepcopy(sets)
            best_nodes = nodes
        end

        #reset global variables
        global pq = PriorityQueue{Node, Float64}()
        global sets = [Set{Node}(), Set{Node}()]
        global unpartitioned_vertices = []
    end

    println("---Wave Function Collapse Partitioning--")
    println("Max Cut Weight: $max_cut_weight")

end


#Benchmarking
function benchmark(graph, edges,  temp = 200, p = .95, a = 200)
    global pq = PriorityQueue{Node, Int}()
    global sets = [Set{Node}(), Set{Node}()]
    global unpartitioned_vertices = []
    subsets, edges = wfc_mc(graph, edges, temp, p, a)
end

function generate_file(num_nodes = 7, edge_multiplier = 1, filename = "kroE100.txt")
    graph, edges = setup(num_nodes, edge_multiplier, filename)
    println("Using file: $filename")
    return graph, edges
end

graph, edges = generate_file()

main(graph, edges)
@benchmark benchmark(graph, edges) #displays performance metrics

        


