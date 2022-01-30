SHELL := /bin/bash
ez:
	uldr_compil uldr_maths ../doc.mdx latex -o TIPE_Ulysse_DURAND.tex tikz -pdf
simple:
	uldr_compil uldr_maths ../doc.mdx latex -o TIPE_Ulysse_DURAND_sansfig.tex -pdf
whole:
	uldr_compil uldr_tikztree figures/tree1.utree -o figures/tree1.tikz
	uldr_compil uldr_tikztree figures/tree2.utree -o figures/tree2.tikz
	uldr_compil uldr_tikztree figures/tree3.utree -o figures/tree3.tikz
	uldr_compil uldr_tikztree figures/tree4.utree -o figures/tree4.tikz
	uldr_compil uldr_tikztree figures/tbu0.utree -o figures/tbu0.tikz
	uldr_compil uldr_tikztree figures/tbu1.utree -o figures/tbu1.tikz
	uldr_compil uldr_tikztree figures/tbu2.utree -o figures/tbu2.tikz
	uldr_compil uldr_tikztree figures/tbu3.utree -o figures/tbu3.tikz
	uldr_compil uldr_maths ../doc.mdx latex -o TIPE_Ulysse_DURAND.tex tikz -pdf
	uldr_compil uldr_maths ../doc.mdx latex -o TIPE_Ulysse_DURAND.tex tikz -pdf