"""
 Copyright (c) 2026 FZI Forschungszentrum Informatik

 SPDX-License-Identifier: Apache-2.0
"""

"""
LCOV Coverage Grouper
Groups coverage data by filename (ignoring paths) and merges coverage statistics
"""

import sys
import os
from collections import defaultdict
from pathlib import Path


def parse_lcov_file(filepath):
    """Parse an LCOV .info file and return grouped coverage data"""
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    records = []
    current_record = {}
    
    for line in lines:
        line = line.strip()
        
        if line.startswith('SF:'):
            # Source file
            current_record = {'path': line[3:], 'lines': {}}
        elif line.startswith('DA:'):
            # Line coverage data (line_num, hit_count)
            parts = line[3:].split(',')
            line_num = int(parts[0])
            hit_count = int(parts[1])
            current_record['lines'][line_num] = hit_count
        elif line.startswith('end_of_record'):
            if current_record:
                records.append(current_record)
                current_record = {}
    
    return records


def group_by_filename(records):
    """Group records by filename (basename) and merge coverage"""
    grouped = defaultdict(lambda: {'paths': [], 'lines': defaultdict(int)})
    
    for record in records:
        filename = os.path.basename(record['path'])
        grouped[filename]['paths'].append(record['path'])
        
        # Merge line coverage (sum hit counts)
        for line_num, hit_count in record['lines'].items():
            grouped[filename]['lines'][line_num] += hit_count
    
    return grouped


def write_grouped_lcov(grouped_data, output_path):
    """Write grouped coverage data to LCOV format"""
    with open(output_path, 'w') as f:
        for filename, data in sorted(grouped_data.items()):
            # Use the first path as representative, or just the filename
            f.write(f"SF:{filename}\n")
            
            # Write line coverage
            for line_num in sorted(data['lines'].keys()):
                hit_count = data['lines'][line_num]
                f.write(f"DA:{line_num},{hit_count}\n")
            
            # Calculate and write summary
            total_lines = len(data['lines'])
            covered_lines = sum(1 for hits in data['lines'].values() if hits > 0)
            
            f.write(f"LF:{total_lines}\n")
            f.write(f"LH:{covered_lines}\n")
            f.write("end_of_record\n")


def print_summary(grouped_data):
    """Print a summary of the grouped coverage"""
    print("\n=== Coverage Summary ===")
    print(f"Total unique filenames: {len(grouped_data)}\n")
    
    for filename, data in sorted(grouped_data.items()):
        total = len(data['lines'])
        covered = sum(1 for hits in data['lines'].values() if hits > 0)
        percentage = (covered / total * 100) if total > 0 else 0
        
        print(f"{filename}:")
        print(f"  Paths merged: {len(data['paths'])}")
        if len(data['paths']) > 1:
            for path in data['paths']:
                print(f"    - {path}")
        print(f"  Coverage: {covered}/{total} lines ({percentage:.1f}%)")
        print()


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python lcov_grouper.py <input.info> [output.info]")
        print("\nGroups coverage by filename, merging data from files with the same name")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else "grouped_coverage.info"
    
    print(f"Processing: {input_file}")
    
    # Parse the LCOV file
    records = parse_lcov_file(input_file)
    print(f"Found {len(records)} file records")
    
    # Group by filename
    grouped = group_by_filename(records)
    
    # Write output
    write_grouped_lcov(grouped, output_file)
    print(f"Wrote grouped coverage to: {output_file}")
    
    # Print summary
    print_summary(grouped)