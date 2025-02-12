import xml.etree.ElementTree as ET

# Load and parse the XML file
tree = ET.parse('path/to/your/.xml/file')
root = tree.getroot()

# Open a file to write the output
with open('path/to/your/.txt/file', 'w') as output_file:

    for i, vertex in enumerate(root.findall('.//vertex')):
        vertex_index = i + 1

        for edge in vertex.findall('edge'):
            connected_vertex = int(edge.text) + 1
            weight = float(edge.get('cost'))
            output_file.write(f"{vertex_index} {connected_vertex} {weight}\n")

