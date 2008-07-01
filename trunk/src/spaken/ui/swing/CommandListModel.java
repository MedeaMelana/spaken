package spaken.ui.swing;

import javax.swing.AbstractListModel;

import spaken.model.Command;
import spaken.model.CommandHistory;
import spaken.model.CommandListener;

public class CommandListModel extends AbstractListModel implements
		CommandListener {

	private CommandHistory history;

	public CommandListModel(CommandHistory history) {
		this.history = history;
	}

	public void commandExecuted(Command c) {
		int n = history.getUndoes().size();
		fireIntervalAdded(this, 0, n - 1);
	}

	public void commandRedone(Command c) {
	}

	public void commandUndone(Command c) {
	}
	
	public Object getElementAt(int index) {
		final Command c = getCommandAt(index);
		return new Object() {

			@Override
			public String toString() {
				return c.getLabel();
			}
			
		};
	}

	public Command getCommandAt(int index) {
		int nundoes = history.getUndoes().size();
		if (index < nundoes) {
			return history.getUndoes().get(index);
		} else {
			index -= nundoes;
			index = history.getRedoes().size() - index - 1;
			return history.getRedoes().get(index);
		}
	}

	public int getSize() {
		return history.getUndoes().size() + history.getRedoes().size();
	}

}
