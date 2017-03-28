{ isSafe ? "no" }: (import ./default.nix { inherit isSafe; }).env
