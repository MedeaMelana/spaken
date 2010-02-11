package spaken.ui.swing;

import javax.swing.DefaultListSelectionModel;

import spaken.model.Command;
import spaken.model.CommandHistory;
import spaken.model.CommandListener;

public class CommandListSelectionModel extends DefaultListSelectionModel
		implements CommandListener {

	private CommandHistory history;

	public CommandListSelectionModel(CommandHistory history) {
		this.history = history;
		history.addListener(this);
		setSelectionMode(SINGLE_SELECTION);
	}

	public void commandExecuted(Command c) {
		update();
	}

	public void commandRedone(Command c) {
		update();
	}

	public void commandUndone(Command c) {
		update();
	}

	private void update() {
		int n = getCurrentIndex();
		if (n < 0) {
			clearSelection();
		} else {
			super.setSelectionInterval(n, n);
		}
	}

	private int getCurrentIndex() {
		return history.getUndoes().size() - 1;
	}

	@Override
	public void setSelectionInterval(int newIndex, int newIndex1) {
		try {
			int oldIndex = getCurrentIndex();
			for (int i = 0; i < Math.abs(newIndex - oldIndex); i++) {
				if (oldIndex < newIndex) {
					history.redo();
				} else {
					history.undo();
				}
			}
		} finally {
			update();
		}
	}

}
