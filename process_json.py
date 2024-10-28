import json
import re

def extract_info(line):
    try:
        # Extrair o total como inteiro
        match = re.search(r'"set".*?"printedTotal",Number (\d+)', line)
        if match:
            total_sets = match.group(1)
            name_match = re.search(r'nameCorreto:([^,]+)', line)
            number_match = re.search(r'numeroCorreto:(\d+)', line)
            quantity_match = re.search(r'quantidade:(\d+)', line)
            if name_match and number_match and quantity_match:
                name = name_match.group(1).strip()
                number = number_match.group(1).zfill(3)
                quantity = quantity_match.group(1)

                # Montar a string formatada com os valores extraídos
                return f"{quantity} {name} ({number}/{total_sets})"
    except Exception as e:
        print(f"Error processing line: {e}")
    return None

def process_file(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    
    formatted_lines = []
    for line in lines:
        info = extract_info(line)
        if info:
            formatted_lines.append(info)
    
    return formatted_lines

def save_results(results, output_file):
    with open(output_file, 'w') as file:
        for idx, result in enumerate(results, 1):
            file.write(f"{result}\n")

if __name__ == "__main__":
    input_file = 'cards.txt'  # Nome do arquivo gerado
    output_file = 'formatted_cards.txt'  # Nome do arquivo de saída
    
    results = process_file(input_file)
    save_results(results, output_file)
    print(f"Resultados salvos em {output_file}")
