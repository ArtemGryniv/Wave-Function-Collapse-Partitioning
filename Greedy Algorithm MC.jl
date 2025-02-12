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
    entropy::Int
    set::Int
    partitioned::Bool
end


"Creates a random graph (as a .txt) as well as a SimpleGraph graph."
function setup(num_nodes, edge_multiplier, filename)
    if filename == ""
        next_available_filename = get_filename()
        println("Using this input file: $next_available_filename")

######## Uncomment the appropriate graph-generating function #########

        make_random_graph(next_available_filename, num_nodes, edge_multiplier)
        #make_clustered_graph(next_available_filename, num_nodes, 2 , .9, .05) #enter values for num_clusters, intra_cluster_prob, inter_cluster_prob
        #make_cycle_graph(next_available_filename, num_nodes) 
        #make_regular_graph(next_available_filename, num_nodes, 25) #enter value for degree
        #make_dense_graph(next_available_filename, num_nodes, 2) #enter value for edge_prob

    else 
        next_available_filename = filename 
    end
    edges = read_data(next_available_filename)
    edge_list = Edge.(edges)
    return SimpleGraph(edge_list), edges
end


"Write a new random graph to a file, where each line is a pair of node keys (e.g. 2 4)."
function make_random_graph(filename, num_nodes, edge_multiplier)
    open(pwd() * "/input_graphs/" * filename, "w") do f
        nodes = [x for x in 1:num_nodes]
        combos = collect(Combinatorics.combinations(nodes, 2))
        edges = collect(sample(combos, trunc(Int, edge_multiplier * num_nodes); replace = false))
        for edge in edges
            write(f, "$(edge[1]) $(edge[2]) \n")
        end
    end
end


"Make Clustered graph"
function make_clustered_graph(filename, num_nodes, num_clusters, intra_cluster_prob, inter_cluster_prob)
    open(pwd() * "/input_graphs/" * filename, "w") do f
        nodes = 1:num_nodes
        cluster_size = num_nodes ÷ num_clusters
        clusters = [collect((i-1)*cluster_size + 1:i*cluster_size) for i in 1:num_clusters]
        
        # Generate intra-cluster edges
        for cluster in clusters
            for (i, node1) in enumerate(cluster)
                for node2 in cluster[i+1:end]
                    if rand() < intra_cluster_prob
                        write(f, "$node1 $node2 \n")
                    end
                end
            end
        end

        # Generate inter-cluster edges
        for i in 1:num_clusters-1
            for j in i+1:num_clusters
                for node1 in clusters[i]
                    for node2 in clusters[j]
                        if rand() < inter_cluster_prob
                            write(f, "$node1 $node2 \n")
                        end
                    end
                end
            end
        end
    end
end


"Make Cycle Graph"
function make_cycle_graph(filename, num_nodes)
    open(pwd() * "/input_graphs/" * filename, "w") do f
        for i in 1:num_nodes
            write(f, "$i $((i % num_nodes) + 1)\n")
        end
    end
end


"Make Regular Graph"
function make_regular_graph(filename, num_nodes, degree)
    if degree > num_nodes - 1 || (degree * num_nodes) % 2 != 0
        error("Degree is too high or incompatible with the number of nodes.")
    end

    max_attempts = 1000  # Limit to prevent infinite loop
    attempts = 0

    open(pwd() * "/input_graphs/" * filename, "w") do f
        nodes = 1:num_nodes
        neighbors = Dict{Int, Set{Int}}(n => Set{Int}() for n in nodes)
        
        while attempts < max_attempts
            for node in nodes
                while length(neighbors[node]) < degree
                    neighbor = sample(nodes)
                    if neighbor != node && !(neighbor in neighbors[node]) && length(neighbors[neighbor]) < degree
                        push!(neighbors[node], neighbor)
                        push!(neighbors[neighbor], node)
                        write(f, "$node $neighbor\n")
                    end
                end
            end

            # Check if all nodes have the required degree
            if all(length(neighbors[node]) == degree for node in nodes)
                return
            end
            # Reset if not all nodes have the required degree (to avoid infinite loops)
            neighbors = Dict{Int, Set{Int}}(n => Set{Int}() for n in nodes)
            attempts += 1
        end

        error("Failed to generate a regular graph within the attempt limit.")
    end
end


"Make Dense Graph"
function make_dense_graph(filename, num_nodes, edge_prob)
    open(pwd() * "/input_graphs/" * filename, "w") do f
        for i in 1:num_nodes
            for j in i+1:num_nodes
                if rand() < edge_prob
                    write(f, "$i $j\n")
                end
            end
        end
    end
end


"Read a graph from a file, where each line is a pair of node keys (e.g. 2 4)"
function read_data(filename)
    edges = Tuple{Int, Int}[]
    io = open(pwd() * "/input_graphs/" * filename, "r");
    for line in eachline(io)
        x, y = [parse(Int, ss) for ss in split(line)]
        t = Tuple([x, y])
        push!(edges, t)
    end
    return edges
end


"Display the given graph."
function viewgraph(graph, nodes)  
    p = graphplot(graph,
        names = 1:nv(graph),
        fontsize = 14,
        nodelabeldist = 5, 
        nodelabelangleoffset = π/4,
        markershape = :circle,
        markersize = 0.15,
        markerstrokewidth = 2,
        markerstrokecolor = :gray,
        edgecolor = :gray,
        linewidth = 2,
        curves = false
    )
    display(p)
end


"Return the first available filename of the form g#.txt from the input_graphs folder."
function get_filename()
    biggest_number = 1
    while isfile(pwd() * "/input_graphs/" * string('g') * string(biggest_number) * ".txt")
        biggest_number = biggest_number + 1
    end
    return string('g') * string(biggest_number) * ".txt"
    # return "g1.txt" # use this for debugging
end


"Creates an array of Node structs as a way of representing the given graph."
function create_nodes_vector(graph)
    nodes = Vector{Node}()
    for key in 1:nv(graph)
        deg = length(all_neighbors(graph, key))
        new_node = Node(key, deg, all_neighbors(graph, key), 0, 0, false)
        push!(nodes, new_node) 
    end
    return nodes
end



function report_wfc(graph, edges, nodes)
    length(edges) < 63 && viewgraph(graph, nodes) 
end


################################################
function greedy_MC(graph)
    sets = [Set{Node}(), Set{Node}()]
    nodes = create_nodes_vector(graph)
    sorted_nodes = sort(nodes, by = x -> x.degree, rev = true)

    push!(sets[1], sorted_nodes[1])
    sorted_nodes[1].set = 1
    popfirst!(sorted_nodes)

    for node in sorted_nodes
        set1_count = 0
        set2_count = 0

        for v in node.neighbors
            if nodes[v].set == 1
                set1_count += 1
            elseif nodes[v].set == 2
                set2_count += 1
            end
        end
    

        if set1_count > set2_count
            good_set = 2
        elseif set1_count < set2_count
            good_set = 1
        else
            good_set = rand(1:2)
        end

        push!(sets[good_set], node)
        node.set = good_set
    end

    return sets, nodes
end
################################################

function calculate_cuts(edges, sets)
    map = Dict{Int, Int}()
    for (index, set) in enumerate(sets)
        for v in set
            map[v.key] = index
        end
    end

    cut_edges = []
    num_cuts = 0
    for edge in edges
        x, y = edge
        if map[x] != map[y]
            num_cuts += 1
            push!(cut_edges, edge)
        end
    end

    return num_cuts, cut_edges
end



#main function
function main(graph, edges, amt_trials = 1000)   
    max_cut_edges = 0
    best_partition = []
    best_nodes = []
    list_of_cuts = []

    for _ in 1:amt_trials
        sets, nodes = greedy_MC(graph)
        num_cuts, cut_edges, = calculate_cuts(edges, sets)
        push!(list_of_cuts, num_cuts)
        
        if num_cuts > max_cut_edges
            max_cut_edges = num_cuts
            best_partition = deepcopy(sets)
            best_nodes = nodes
        end

        #reset global variables
        global sets = [Set{Node}(), Set{Node}()]
    end

    println("---Greedy--")
    println("Max Cut Weight: $max_cut_edges")
    report_wfc(graph, edges, best_nodes)

end


#Benchmarking
function benchmark(graph, edges)
    subsets, nodes = greedy_MC(graph)
end

function generate_file(num_nodes = 7, edge_multiplier = 1, filename = "")
    graph, edges = setup(num_nodes, edge_multiplier, filename)
    return graph, edges
end
graph, edges = generate_file()


main(graph, edges)
@benchmark benchmark(graph, edges)