use Amnesia

defdatabase IRCd.Database do

  # Configuration table
  deftable Config, [:key, :value]

  # User! not used yet
  deftable User,
    [ :id, :nick, :user, :realname, :host, :ip, :registered,
      :modes, :oper, :connected_at, :registered_at, :activity_at ],
      type: :ordered_set, index: [:nick] do

  end
end
