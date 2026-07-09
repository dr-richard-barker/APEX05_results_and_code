# Arabidopsis cell-type marker sets (derived from PCMDB)

Source: **Plant Cell Marker DataBase (PCMDB)**, Jin *et al.*, *Nucleic Acids Research* 2022; data DOI **10.5281/zenodo.5101271** (`PCMDB_all_maker_info.xlsx`).

Built by `analysis/ml/apex05_celltype_markers.py`. Each marker maps to a single cell type (specificity-filtered). Root cell types come from the curated *Experimental* markers; leaf/shoot vegetative cell types from the *SingleCellSeq* markers restricted to the leaf set. Columns: `gene` (AGI), `cell_type`, `tissue`, `source`. Used for cell-type marker scoring and flight-DEG cell-type enrichment (`apex05_celltype_deconvolution.py`).
