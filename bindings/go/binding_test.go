package tree_sitter_move_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-move"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_move.Language())
	if language == nil {
		t.Errorf("Error loading Move grammar")
	}
}
